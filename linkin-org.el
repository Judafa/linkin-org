;;; linkin-org.el --- An emacs workflow with fast, reliable links

;; Author: Julien Dallot <judafa@protonmail.com>
;; URL: https://github.com/Judafa/linkin-org
;; Version: 0.1

;; This file is not part of GNU Emacs

;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

;; (require 'async)
;; (require 'cl-lib)
;; (require 'org)



;;;; -------------------------------------------- main variables

;; define the store directory as the user's directory by default
(defcustom linkin-org-store-directory (expand-file-name "~/") "the directory to store your files or directories with linkin-org")

;; define the directories where to search when a link is broken
(defcustom linkin-org-search-directories-to-resolve-broken-links (list (expand-file-name "~/")) "the list of directories to search into when links are broken")


;; list of link types such that, when following the link, the id should be checked if the link does not work
(defcustom linkin-org-link-types-to-check-for-id
  '("mpd" "pdf" "video" "file")
  "list of link types such that, when following the link, the id should be checked if the link does not work"
  )



;;;; -------------------------------------------- patterns


;; regexp recognizing an id
(defconst linkin-org-id-regexp
  (rx
   (or
    ;; denote style
    (seq
     ;; the timestamp
     (= 4 digit) (= 2 digit) (= 2 digit) "T" (= 2 digit) (= 2 digit) (= 2 digit)
     ;; the signature
     (? (seq "==" (* alnum)))
     )

    ;; org-roam style
    (seq (= 4 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit))

    ;; some stuff I tried at the beginning
    (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "--" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) (or (seq) (seq "--" (= 5 digit))))
    )
   )
  )



(defun linkin-org-create-id ()
  "Return an id in Denote style, which is a string with the current year, month, day, hour, minute, second, and milliseconds."
  (let*
      (
       (time-string (format-time-string "%Y%m%dT%H%M%S" (current-time)))
       )
    time-string
    )
  )

;; regexp recognizing a separator between id and original filename
(defconst linkin-org-sep-regexp
  (rx (or "--" "-"))
  )

;; separator between the id and the original file name
(defconst linkin-org-sep
  "--"
  )



;;;; ------------------------------------------- basic functions

(defun linkin-org-get-id (file-name)
  "if the given file name has an id in it then returns it, nil otherwise"
  (if (string-match linkin-org-id-regexp file-name)
      ;; this function returns a list of list of strings
      (car (car (s-match-strings-all linkin-org-id-regexp file-name)))
    nil
    )
  )

(defun linkin-org-strip-off-id-from-file-name (file-name)
  "take a file name and strip off the id part"
  (let*
      (
       (id (linkin-org-get-id file-name))
       )
    (if id
        (let*
            (
	         ;; remove the id
	         (file-name-without-id (replace-regexp-in-string id "" file-name))
	         ;; remove the heading sep -- if there is one
	         (file-name-without-sep (replace-regexp-in-string (concat "^" linkin-org-sep-regexp) "" file-name-without-id))
             )
            )
      file-name
        )
    )
  )

(defun linkin-org-transform-square-brackets (str)
  "Escape occurrences of '\\\\', '\\[', and '\\]' in INPUT string."
  (let
      ((new-string
	(replace-regexp-in-string
	 "\\[" "\\\\["
	 (replace-regexp-in-string
	  "\\]" "\\\\]"
	  ;; (replace-regexp-in-string "\\\\" "\\"
	  str
	  ;; )
	  )
	 )
	))
    ;; (message new-string)
    new-string
    )
  )


(defun linkin-org-link-escape (link-string)
  (replace-regexp-in-string
   (rx (seq (group (zero-or-more "\\")) (group (or string-end (any "[]")))))
   (lambda (m)
     (concat (match-string 1 m)
	     (match-string 1 m)
	     (and (/= (match-beginning 2) (match-end 2)) "\\")))
   link-string nil t 1)
  )

(defun linkin-org-parse-org-link (link-string)
  "Parse LINK-STRING into an Org element and return the result."
  (with-temp-buffer
   (let ((org-inhibit-startup nil))
     (insert link-string)
     (org-mode)
     (goto-char (point-min))
     (org-element-link-parser))
   )
  )



(defun linkin-org-package-installed-p (pkg-name)
  "Return t if a package named PKG-NAME is installed. Launches the package's command-line name followed by --version to find out."
  (let (
        (cmd
         (format "%s --version" pkg-name)
         )
        )
    (eq (call-process-shell-command cmd) 0)
    )
  )

(defun linkin-org-find-matching-file (id dirs &optional search-depth search-type)
  "Use the best installed searching software to look for a file matching ID in the directories in DIR.
DIRS is a list of directories paths, it will try to find the file it each directories in DIRS in order, stops when one match is found.
Search for search-depth levels in dir, search in all directories recursively if search-depth is nil.
If search-type is 'file then only matches files, if search-type is 'directory then only matches directories, if search-type is nil then matches both files and directories.
Returns the file path as a string or nil if not found."
    (cond
     (
      ;; try first with the fd command if installed on the local machine
      (linkin-org-package-installed-p "fd")
      
      (let (
            (tmp-dirs dirs)
            (file-found-p (if dirs
                              'search-in-progress
                            'not-found
                            )
                          )
            resolved-file-name
            )
        ;; try for each dir in dirs
        (while (eq file-found-p 'search-in-progress)
          (let* (
	             (dir (expand-file-name (car tmp-dirs)))
                 (search-depth-flag (if (integerp search-depth)
                                        (format "--max-depth=%s" search-depth)
                                      nil
                                      )
                                    )
                 (search-type-flag (cond
                                    ((eq search-type 'file) "--type=file")
                                    ((eq search-type 'directory) "--type=directory")
                                    (t nil)
                                    )
                                   )
                 (base-directory-flag (format "--base-directory=%s" dir))
	             ;; (fd-command (format "(fd %s %s --base-directory=%s %s) | head -n 1" search-depth-flag search-type-flag dir id))
	             ;; file-path
                 )
            ;; consumes the first element of tmp-dirs
            (setq tmp-dirs (cdr tmp-dirs))


            (message (concat "the base directory : " (prin1-to-string base-directory-flag)))
            (message (concat "the id : " (prin1-to-string id)))
            (message (concat "the search depth flag : " (prin1-to-string search-depth-flag)))
            (message (concat "the type flag : " (prin1-to-string search-type-flag)))

            ;; call to fd
            (with-temp-buffer
              (let
                  (
                   ;; (found? (call-process-shell-command fd-command nil (current-buffer) nil))
                   ;; (found? (apply 'call-process "fd"
                   ;;                nil (current-buffer) nil
                   ;;                (-filter (lambda (e) e)
                   ;;                 '(search-depth-flag
                   ;;                   search-type-flag
                   ;;                   base-directory-flag
                   ;;                   id
                   ;;                   )
                   ;;                 )
                   ;;                )
                   ;;         )
                   (found?
                    (cond
                     (
                      (and search-depth-flag search-type-flag)
                      (call-process "fd" nil (current-buffer) nil
                                    base-directory-flag
                                    id
                                    )
                      )
                     (
                      (and (not search-depth-flag) search-type-flag)
                      (call-process "fd" nil (current-buffer) nil
                                    search-type-flag
                                    base-directory-flag
                                    id
                                    )
                      )
                     (
                      (and search-depth-flag (not search-type-flag))
                      (call-process "fd" nil (current-buffer) nil
                                    search-depth-flag
                                    base-directory-flag
                                    id
                                    )
                      )
                     (
                      (and (not search-depth-flag) (not search-type-flag))
                      (call-process "fd" nil (current-buffer) nil
                                    base-directory-flag
                                    id
                                    )
                      )
                     )
                    )
                   )
                (message (concat "found value : " (prin1-to-string found?)))
                (message (buffer-substring-no-properties (point-min) (point-max)))
                (if (and (eq found? 0) (not (zerop (buffer-size))))
                  ;; (setq file-path (car (string-lines (buffer-string))))
                  (setq resolved-file-name (car (string-lines (buffer-string))))
	              )
                )
              )
            ;; if we found the file
            (if resolved-file-name
                (setq file-found-p 'found)
                )

            ;; if we tried all directories in dirs, stop the while loop
            (if (eq tmp-dirs nil)
                (setq file-found-p 'not-found)
              )
            (message (prin1-to-string tmp-dirs))
            
            )
          )
        ;; return the resolved file name, nil if not found
        resolved-file-name
        )
   ;; (
   ;;  ;; else, try with a mix of ripgrep and find
   ;;  (linkin-org-package-installed-p "rg")
   ;;  (let* (
   ;;         (dir (expand-file-name dir))
   ;;         (rg-command (format "(rg -g \"%s*\" --files %s & find \"%s\" -type d -name \"%s*\") | head -n 1" id dir dir id))
   ;;         file-path
   ;;         )
   ;;    (with-temp-buffer
   ;;      (call-process-shell-command rg-command nil (current-buffer) nil)
   ;;      (unless (zerop (buffer-size))
   ;;        (setq file-path (car (string-lines (buffer-string))))
   ;;        )
   ;;      )
   ;;    file-path
   ;;    )
   ;;  )
      )
     (
    ;; else as a last resort, list all files in the file directory with list. quite slow
    t
    (message "todo, install fd for now!")
    ;; (dolist
	;;     ;; third t in directory-files-recursively is to include directories
	;;     (tmp-file (directory-files-recursively file-dir ".*" t t) result)
	;;   ;; Ensure the file or directory exists
	;;   (let
	;; 	  (
	;; 	   (tmp-file-name
	;; 	    (if (equal (file-name-directory tmp-file) tmp-file)
	;; 	        ;; if the file path is a directory
	;; 	        (file-name-nondirectory (directory-file-name tmp-file))
	;; 	      ;; else if the file path is a file
	;; 	      (file-name-nondirectory tmp-file)
	;; 	      )
	;; 	    )
	;; 	   )
	;;     (when (and (file-exists-p tmp-file)
	;; 		       (string-match id-of-file-name tmp-file-name)
	;; 		       )
    ;;       (setq result tmp-file)
	;;       )
	;;     )
	;;   )
    )
   )
  )


(defun linkin-org-resolve-file-path (file-path)
  "returns the path to the file if it exists, use id ultimately to find the file
Use this function if you already checked that the file path is not valid.
"
  (let* (
         ;; equals 'directory if file-path is the path of a directory with a trailing slash, else equals 'file
         (file-or-directory? (if (equal (file-name-directory file-path) file-path)
                                 'directory
                               'file
                               )
                             )
	     ;; get the directory of file-path
	     (file-dir
	      (if (equal file-or-directory? 'directory)
		      ;; if the file path is a directory
		      (file-name-directory (directory-file-name file-path))
	        ;; else if the file path is a file
	        (file-name-directory file-path)
	        )
	      )
	     ;; get the file name
	     (file-name
	      (if (equal file-or-directory? 'file)
		      ;; if the file path is a directory
		      (file-name-nondirectory (directory-file-name file-path))
	        ;; else if the file path is a file
	        (file-name-nondirectory file-path)
	        )
	      )
	     ;; get the id of the file, if it has one
         ;; if the file has no id, let the file-name (without directory) be the id
	     (id-of-file-name
          (let
              ((id (linkin-org-get-file-name-id file-name)))
            (if id
                id
              file-name
                )
            )
          )
         resolved-file-path
	     )
    
    ;; try to resolve the lost file
    (cond
     ;; first, look if the file was just renamed but is still in the same directory
     (
      (progn
        (setq resolved-file-path (linkin-org-find-matching-file
                                  id-of-file-name
                                  (list file-dir)
                                  1
                                  file-or-directory?
                                  )
              )
        resolved-file-path
        )
      ;; return the resolved path
      (message (concat "the first resolved path" (prin1-to-string resolved-file-path)))
      resolved-file-path
      )
     ;; if the file was moved somewhere else, search in the directories in linkin-org-search-directories-to-resolve-broken-links
     (
      (progn
        (setq resolved-file-path (linkin-org-find-matching-file
                                  id-of-file-name
                                  linkin-org-search-directories-to-resolve-broken-links
                                  nil
                                  file-or-directory?
                                  )
              )
        resolved-file-path
        )
      resolved-file-path
      )
     )
	)
  )



;;;; ------------------------------------------- find the linked file
;;;; ------------------------------------------- file link
;;;; ------------------------------------------- music link
;;;; ------------------------------------------- pdf link
(require 'pdf-tools)

;;;; ------------------------------------------- video link
;;;; ------------------------------------------- general purpose functions
;;;; ------------------------------------------- open link
;;;; ------------------------------------------- create link
;;;; ------------------------------------------- inline ids


