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
(defcustom linkin-org-store-directory '(expand-file-name "~/") "the directory to store your files or directories with linkin-org")


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

(defun linkin-org-find-matching-file (id dir &optional search-depth search-type)
  "Use the best installed searching software to look for a file matching ID in DIR.
Search for search-depth levels in dir, search in all directories recursively if search-depth is nil.
If search-type is 'file then only matches files, if search-type is 'directory then only matches directories, if search-type is nil then matches both files and directories.
Returns the file path as a string or nil if not found."
  (cond
   (
    ;; try with the fd command, the fastest
    (linkin-org-package-installed-p "fd")
    (let* (
	       (dir (expand-file-name dir))
           (search-depth-flag (if (integerp search-depth)
                                  (format "--base-directory=%s" search-depth)
                                ""
                                )
                              )
           (search-type-flag (cond
                              ((eq search-type 'file) "--type=file")
                              ((eq search-type 'directory) "--type=directory")
                              (t "")
                              )
                             )
	       (fd-command ((format "(fd %s %s --base-directory=%s %s) | head -n 1" search-depth-flag search-type-flag dir id)))
	       file-path
           )
      
      (with-temp-buffer
        (call-process-shell-command fd-command nil (current-buffer) nil)
        (unless (zerop (buffer-size))
          (setq file-path (car (string-lines (buffer-string))))
	      )
        )
      file-path
      )
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
   (
    ;; else as a last resort, list all files in the file directory with list. quite slow
    t
    (dolist
	    ;; third t in directory-files-recursively is to include directories
	    (tmp-file (directory-files-recursively file-dir ".*" t t) result)
	  ;; Ensure the file or directory exists
	  (let
		  (
		   (tmp-file-name
		    (if (equal (file-name-directory tmp-file) tmp-file)
		        ;; if the file path is a directory
		        (file-name-nondirectory (directory-file-name tmp-file))
		      ;; else if the file path is a file
		      (file-name-nondirectory tmp-file)
		      )
		    )
		   )
	    (when (and (file-exists-p tmp-file)
			       (string-match id-of-file-name tmp-file-name)
			       )
          (setq result tmp-file)
	      )
	    )
	  )
    )
   )
  )


(defun linkin-org-resolve-file-path (file-path)
  "returns the path to the file if it exists, use id ultimately to find the file
Use this function if you already checked that the file path is not valid.
"
  (cond
   ;; else, use id if the file has an id
   (
    (let* (
	   ;; get the directory of file-path
	   (file-dir
	    (if (equal (file-name-directory file-path) file-path)
		;; if the file path is a directory
		(file-name-directory (directory-file-name file-path))
	      ;; else if the file path is a file
	      (file-name-directory file-path)
	      )
	    )
	   ;; get the file name
	   (file-name
	    (if (equal (file-name-directory file-path) file-path)
		;; if the file path is a directory
		(file-name-nondirectory (directory-file-name file-path))
	      ;; else if the file path is a file
	      (file-name-nondirectory file-path)
	      )
	    )
	   ;; get the id of the file, if it has one
	   (id-of-file-name (linkin-org-get-file-name-id file-name))
	   result
	   )

      ;; try to find the lost file only if it has an id
      (if id-of-file-name (find-first-matching-file id-of-file-name file-dir)
	    )
      )
    )
   ;; if the id search failed, just return the file path
   (t
    file-path
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


