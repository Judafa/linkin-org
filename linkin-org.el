;; linkin-org.el --- An emacs workflow with fast, reliable links

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



(require 'ol)


;;;; -------------------------------------------- main variables

;; define the directory where linkin-org-store stores the files/directories by default
(defcustom linkin-org-store-directory (expand-file-name "~/") "The directory where linkin-org-store stores data by default.")

;; define the directories where to search when a link is broken
(defcustom linkin-org-search-directories-to-resolve-broken-links (list (expand-file-name "~/")) "The list of directories to search into when a link is broken")

;; list of link types such that, when following the link, the id should be checked if the link does not work
(defcustom linkin-org-link-types-to-check-for-id
  '("pdf" "file")
  "List of link types such that, if the link is broken, the id in the link should be used to resolve the link"
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
     ;; the signature, if there is one
     (? (seq "==" (* alnum)))
     )

    ;; org-roam style
    (seq (= 4 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit))

    ;; some stuff I tried at the beginning
    (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "--" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) (or (seq) (seq "--" (= 5 digit))))
    )
   )
  )


;; regexp recognizing an inline id.
;; inline ids are ids written directly in the file.
;; their id must be distinctive from file id since we dont want links to be identified as inline ids; just add "id:" in front of it.
(defconst linkin-org-inline-id-regexp
  (concat "id:" linkin-org-id-regexp)
  )



(defun linkin-org-create-id ()
  "Return an id in Denote style, which is a string with the current year, month, day, hour, minute, second"
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

(defun linkin-org-get-id (s &optional id-regexp)
  "if the given string contains an id then returns it, nil otherwise"
  (unless id-regexp
    (setq id-regexp linkin-org-id-regexp)
    )
  (if (string-match id-regexp s)
      ;; this function returns a list of list of strings
      (car (car (s-match-strings-all id-regexp s)))
    nil
    )
  )

(defun linkin-org-strip-off-id-from-file-name (file-name)
  "take a file name and strip off the id part"
  (let*
      (
       (id (linkin-org-get-id file-name linkin-org-id-regexp))
       )
    (if id
        (let*
            (
	         ;; remove the id
	         (file-name-without-id (replace-regexp-in-string id "" file-name))
	         ;; remove the heading sep -- if there is one
	         (file-name-without-sep (replace-regexp-in-string (concat "^" linkin-org-sep-regexp) "" file-name-without-id))
             )
          file-name-without-sep
            )
      file-name
        )
    )
  )

(defun linkin-org-give-id-to-file-name (file-name)
  "Take a file name FILE-NAME (without path) and return a new file name with id.
Does not add an id if FILE-NAME already has one.
"
  (if (linkin-org-get-id file-name)
      ;; if the file already has an id, dont add one
      file-name
    ;; else add an id
    (concat (linkin-org-create-id) linkin-org-sep file-name)
      )
    )

;; [id:20250408T202457] 
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
    new-string
    )
  )


(defun linkin-org-link-escape (string-link)
  (replace-regexp-in-string
   (rx (seq (group (zero-or-more "\\")) (group (or string-end (any "[]")))))
   (lambda (m)
     (concat (match-string 1 m)
	     (match-string 1 m)
	     (and (/= (match-beginning 2) (match-end 2)) "\\")))
   string-link nil t 1)
  )

(defun linkin-org-parse-org-link (string-link)
  "Parse STRING-LINK into an Org element and return the result."
  (with-temp-buffer
   (let ((org-inhibit-startup nil))
     (insert string-link)
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


(defun linkin-org-resolve-file-with-path (file-path)
  "Starting at the root directory, climb up the file-path directory by directory.
Whenever the current subpath is not valid, resolves using id.
FILE-PATH is the file path of a file or a directory.
Returns the resolved file path, nil if it could not resolve it.
This always finds your file back if you only renamed files and preserved the ids; it does not work if you changed the location of some directories in file-path.
It is assumed you already checked that file-path is not valid.
"
  (let*
      (
       ;; expand file path
       (file-path (expand-file-name file-path))
       ;; the directory in construction
       (building-dir "/")
       ;; split the dir into all its intermediary directories
       ;; doesnt work on windows!
       (split-path (split-string file-path "/") )
       ;; remove empty strings ""
       (split-path (seq-remove
                    'string-empty-p
                    split-path
                    )
                   )
       )
    (dolist (sub-dir split-path)
      
      ;; building-dir is set of nil as soon as we know the path cannot be resolved.
      (when building-dir
       (let
           (
            (tmp-building-dir (concat (file-name-as-directory (directory-file-name building-dir)) sub-dir))
            resolved-dir
            )
         (if (file-exists-p tmp-building-dir)
             ;; if the subdir is already valid, just pile it up
             (setq building-dir tmp-building-dir)
           ;; else, try to resolve it with id
           (if-let*
               ;; get the id of the considered file/dir, if it exists
               (
                (id (linkin-org-get-id sub-dir linkin-org-id-regexp))
                )
               ;; if the file has an id, try to resolve it
               (cond
                (
                 ;; try with fd, if installed
                 (linkin-org-package-installed-p "fd")
                 (with-temp-buffer
                   (let
                       (
                        (found?
                         (call-process "fd" nil (current-buffer) nil
                                       (format "--base-directory=%s" building-dir)
                                       ;; just search by breadth in the current directory, not recursively
                                       "--max-depth=1"
                                       id
                                       )
                         )
                        )
                     ;; collect the results
                     (when (and (eq found? 0) (not (zerop (buffer-size))))
                       (setq resolved-dir
                             ;; just take the first match
                             (car (string-lines (buffer-string)))
                             )
                       )
                     )
                   )
                 (if resolved-dir
                     ;; if we found a match, append the resolved dir to the building dir
                     (setq building-dir
                           (if (file-exists-p resolved-dir)
                               ;; I do this since I'm never sure whether fd returns an absolute or relative path
                               resolved-dir
                             (concat (file-name-as-directory (directory-file-name building-dir)) resolved-dir)
                             )
                           )
                   ;; if we did not find a match, then we cannot resolve the file. set the buidling path to nil
                   (setq building-dir nil)
                   )
                 )
                (t
                 ;; else, just use emacs-lisp code to find a matching id. slowest option
                 (setq building-dir
                       (car
                        (-filter
                         (lambda (s)
                                 (or
                                  (not (s-equals? s "."))
                                  (not (s-equals? s ".."))
                                  )
                                 )
                         (directory-files building-dir t id t)
                         )
                        )
                       )
                 ;; (let
                 ;;     (
                 ;;      (file-list (directory-files building-dir))
                 ;;      matching-id-found
                 ;;      current-file-to-investigate
                 ;;      resolved-dir
                 ;;      )
                 ;;   ;; go over each file and check for id
                 ;;   (while (and
                 ;;           (not matching-id-found)
                 ;;           file-list
                 ;;           )
                 ;;     ;; set the current file to investigate
                 ;;     (setq current-file-to-investigate (car file-list))
                 ;;     ;; remove that file from the list of files
                 ;;     (setq file-list (cdr file-list))
                 ;;     ;; stop the search if the file contains the id
                 ;;     (when (string-match id current-file-to-investigate)
                 ;;       (setq matching-id-found t)
                 ;;       (setq resolved-dir current-file-to-investigate)
                 ;;       )
                 ;;     )
                 ;;   (setq building-dir (concat
                 ;;                       (file-name-as-directory (directory-file-name building-dir))
                 ;;                       resolved-dir
                 ;;                       )
                 ;;         )
                 ;;   )
                 )
                )
             ;; else if there is no id, then we cannot resolve the file. set the buidling path to nil
             (setq building-dir nil)
             )
           )
         )
       )
      )
    building-dir
    )
  )


(defun linkin-org-resolve-file-with-store-directory (file-path &optional directories-to-look-into)
  "Searches for files contained in directories-to-look-into recursively and returns a file that has the same id as that of file-path.
returns nil if file-path has no id or if no matching file was found.
File-path can be the path of a file or a directory.
If not provided, directories-to-look-into is set to the default linkin-org-search-directories-to-resolve-broken-links.
It is assumed you already checked that file-path is not valid.
"
  (let*
      (
       ;; expand file path
       (file-path (expand-file-name file-path))
       ;; set the list of directories to looki into to the default if it was not provided
       (directories-to-look-into (if directories-to-look-into
                                     directories-to-look-into
                                   linkin-org-search-directories-to-resolve-broken-links
                                     )
                                 )
       ;; get the name and id of the file or directory to look for
	   (file-name
        (if (file-directory-p file-path)
		    ;; if the file path is that of a directory
		    (file-name-nondirectory (directory-file-name file-path))
		  ;; else if the file path is a file
		  (file-name-nondirectory file-path)
		  )
        )
       (id (linkin-org-get-id file-name))
       
       (file-found-p (if directories-to-look-into
                         'search-in-progress
                       'not-found
                       )
                     )
       (tmp-dirs directories-to-look-into)
       resolved-file-path
       )
    ;; try for each dir in directories-to-look-into
    (while (eq file-found-p 'search-in-progress)
      ;; take one directory in the directories to look into
      (let* ((dir (expand-file-name (car tmp-dirs))))
        
        ;; consumes the head directory in tmp-dirs
        (setq tmp-dirs (cdr tmp-dirs))
        ;; check if dir is a valid directory.
        ;; if not, try to resolve it with ids
        (unless (file-exists-p dir)
          (setq dir (linkin-org-resolve-file-with-path dir))
          )
        ;; if dir exists or could be resolved
        (when dir
          (cond
           (
            ;; try with fd, if installed
            (linkin-org-package-installed-p "fd")
            (with-temp-buffer
              (let
                  (
                   (found?
                    (call-process "fd" nil (current-buffer) nil
                                  (format "--base-directory=%s" dir)
                                  id
                                  )
                    )
                   )
                ;; collect the results
                (when (and (eq found? 0) (not (zerop (buffer-size))))
                  (setq resolved-file-path
                        ;; just take the first match
                        (car (string-lines (buffer-string)))
                        )
                  )
                )
              )
            ;; if we found a match, the search is over
            (when resolved-file-path
                (setq file-found-p 'found)
                ;; I do this since I'm never sure whether fd returns an absolute or relative path
                (setq resolved-file-path
                      (if (file-exists-p resolved-file-path)
                          resolved-file-path
                        (concat (file-name-as-directory (directory-file-name dir)) resolved-file-path)
                        )
                      )
                )
            ;; if we found no match and if we looked into all the dirs
            (when (and (not resolved-file-path) (not tmp-dirs))
              (setq file-found-p 'not-found)
              )
            )
           (t
            ;; (let
            ;;     (
            ;;      (file-list-rec (directory-files-recursively dir id))
            ;;      )
            ;;     )
            (setq resolved-file-path
                  (car
                   (-filter
                    ;; get rid of the stupid "." and ".." files
                    (lambda (s)
                      (or
                       (not (s-equals? s "."))
                       (not (s-equals? s ".."))
                       )
                      )
                    (directory-files-recursively dir id t)
                    )
                   )
                  )
            ;; if we found a match, the search is over
            (when resolved-file-path
              (setq file-found-p 'found)
              )
            ;; if we found no match and if we looked into all the dirs
            (when (and (not resolved-file-path) (not tmp-dirs))
              (setq file-found-p 'not-found)
              )
            )
           )
          )
        )
      )
    ;; return the resolved path if it was found, else return nil
    (unless (eq file-found-p 'not-found)
      resolved-file-path
     )
    )
  )

(defun linkin-org-resolve-file (file-path)
  "Try different approaches to resolve the file path"
  (cond
   ;; if the path is already correct, do nothing
   ((file-exists-p file-path) file-path)
   ;; else, try resolving the file path with just ids
   ((linkin-org-resolve-file-with-path file-path))
   ;; else, try resolving the file path looking inside store directories
   ((linkin-org-resolve-file-with-store-directory file-path))
   ;; else, file could not be resolved
   (t (message "Neither the file nor the id could be found"))
   )
  )

(defun linkin-org-resolve-link (string-link)
  "Take a link in string form and returns the same link but with a correct path.
only modify the link if its type is in linkin-org-link-types-to-check-for-id.
"
  
  (let*
	  (
       ;;turn the string link into an org element
       (link-org-element (linkin-org-parse-org-link string-link))
       ;; get the raw link, that is, the string containing the data of the link
	   (link-raw-link (org-element-property :raw-link link-org-element))
       ;; get the type of the link
       (link-type (org-element-property :type link-org-element))
	   ;; (link-raw-path (org-element-property :path link-org-element))
	   ;; get data of the link, that is, the interior of the link minus the type (ie, file:)
	   (link-path (org-element-property :path link-org-element))
       ;; extract the path, that is, the substring of link-path before the first :: if there is one
	   (link-path (car (string-split link-path "::")))
       ;; get the substring after the first "::"
	   (link-metadata (let 
                          (
                           (index (string-match "::" link-raw-link))
                           )
                        (if index
                            (substring link-raw-link index)
                          )
                        )
                      )
	   ;; change the path to a correct path
	   (new-link-path (if link-path (linkin-org-resolve-file link-path)))
	   ;; build a new link based on the correct path
	   (new-string-link (concat "[[" link-type ":" (linkin-org-link-escape (concat new-link-path link-metadata)) "]]"))
	   )
	new-string-link
	)
  )


(defun linkin-org-get-org-string-link-under-point ()
  "Returns the string of an org link under point, returns nil if no link was found"
  (if (org-in-regexp
       org-link-any-re
       (let ((origin (point)))
	 (max
	  (save-excursion
            (backward-paragraph)
            (count-lines (point) origin))
	  (save-excursion
            (forward-paragraph)
            (count-lines origin (point))))))
      (match-string-no-properties 0)
    )
  )



;; to do an action on a file as if the point was on that file in dired
(defun linkin-org-perform-function-as-if-in-dired-buffer (file-path function-to-perform)
  "Do an action on a file as if the point was on that file in dired"
  (let*
      (
       ;; get the full path
       (file-path (expand-file-name file-path))
       ;; get the list of dired buffer that already visit the directory of the file
       (dired-buffers-visiting-path (dired-buffers-for-dir (file-name-directory file-path)))
       ;; create a dired buffer visiting the directory of the file (or get the name of it if it already exists)
       (dired-buffer (dired-noselect (file-name-directory file-path)))
       ;; clone the dired buffer
       (cloned-dired-buffer (with-current-buffer dired-buffer (clone-buffer)))
       )
    ;; switch to the cloned dired buffer
    (switch-to-buffer cloned-dired-buffer)
    ;; update the cloned dired buffer
    (revert-buffer)
    ;; place the point on the file
    (dired-goto-file file-path)
    ;; do the function
    (funcall function-to-perform)
    ;; kill the cloned dired buffer
    (kill-buffer cloned-dired-buffer)
    ;; close the dired buffer if it was open in the first place
    (unless dired-buffers-visiting-path
      (kill-buffer dired-buffer)
      )
    )
  )
   

;; to open a file as if in dired
(defun linkin-org-open-file-as-in-dired (file-path)
  "Open a file as if it were opened from dired"
  (linkin-org-perform-function-as-if-in-dired-buffer file-path 'dired-open-file)
  ;; (find-file file-path)
  )

;; to yank the link of a given file
(defun linkin-org-yank-link-of-file (file-path)
  "Yank the link of a given file, as if linkin-org-dired-get-link was called from dired"
  (linkin-org-perform-function-as-if-in-dired-buffer file-path 'linkin-org-dired-get-link)
  )



(defun linkin-org-store-file (&optional yank-link? ask-for-name-confirmation?)
  "Store the file under point in dired"
  (let* (
	     (file-path (dired-file-name-at-point))
         ;; is the file already in the store directory
	     (is-file-already-in-store-directory? (s-prefix?
					                           (expand-file-name linkin-org-store-directory)
					                           (expand-file-name file-path)
					                           )
					                          )
	     )
    ;; check wether it's a file or a directory
    (if (file-directory-p file-path)
	    ;; if it's a directory
	    (progn
	      (let*
	          (
	           ;; ask for the directory new name
	           ;; ~directory-file-name~ removes the trailing slash so that ~file-name-nondirectory~ returns the last part of the path
	           (new-file-name
                (if ask-for-name-confirmation?
                    (read-string "New name: " (file-name-nondirectory (directory-file-name file-path)))
                  (file-name-nondirectory (directory-file-name file-path))
                  )
                )
               ;; give the file name an id
               (new-file-name (linkin-org-give-id-to-file-name new-file-name))
	           )
		    (copy-directory
             file-path
             (if is-file-already-in-store-directory?
                 ;; if the directory is already in the store directory, just rename it without moving it
                 (concat
                  ;; this just concats a "/" at the end of the directory (if there's none already)
                  (file-name-as-directory
                   (file-name-directory (expand-file-name (directory-file-name file-path)))
                   )
                  new-file-name
                  )
               ;; else, move the directory is no in the store directory, move it in there
               ;; (copy-directory file-path (linkin-org-store-directory new-file-name))
               (concat (file-name-as-directory
                        (expand-file-name (directory-file-name linkin-org-store-directory))
                        )
                       new-file-name
                       )
               )
             )
            ;; update the dired buffer
            (revert-buffer)
	        )
	      )
      ;; if it's a file
      (progn
	    (let*
	        ;; ask for the file new name
	        (
	         (new-file-name
              (if ask-for-name-confirmation?
                  (read-string "New name: " (file-name-nondirectory file-path))
                (file-name-nondirectory file-path)
                )
              )
             ;; give the file name an id
	         (new-file-name (linkin-org-give-id-to-file-name new-file-name))
	         (complete-file-path (if is-file-already-in-store-directory?
                                     (concat (file-name-directory (expand-file-name file-path)) new-file-name)
                                   (concat (file-name-as-directory linkin-org-store-directory) new-file-name)
                                   )
                                 )
	         )
          (copy-file file-path complete-file-path)
	      ;; (if is-file-already-in-fourre-tout?
	      ;;     ;; (rename-file file-path complete-file-path)
	      ;;     (rename-file file-path (concat (file-name-directory (expand-file-name (directory-file-name file-path))) id new-file-name))
	      ;; (copy-file file-path complete-file-path)
	      ;;   )
	      (linkin-org-yank-link-of-file complete-file-path)
          ;; update the dired buffer
          (revert-buffer)
	      )
	    )
      )
    )
  )

(defun linkin-org-follow-link-and-do-function (string-link function-to-perform)
  "follow the link, apply the function, come back."
  (let (
        ;; remember the current buffer and the current position of poin
        (init-buffer (current-buffer))
        (init-point (point))
        ;; save the current buffer list
        (init-buffer-list (buffer-list))
        new-buffer
        )
    (unwind-protect
        (progn
          ;; follow to the link
          (linkin-org-follow-string-link link)
          ;; call the function
          (funcall function-to-perform)
          ;; remember the buffer we landed in by following the link
          (setq new-buffer (current-buffer))
         )
      ;; go back where we were
      (switch-to-buffer init-buffer)
      (goto-char init-point)
      ;; kill all buffers that were open
      (mapcar
       '(progn (save-buffer) (kill-buffer))
       (cl-set-difference (buffer-list) init-buffer-list)
       )
      )
    )
  )


(defun linkin-org-follow-string-link (string-link)
  "Open the link STRING-LINK given in string form"
  (if-let*
      (
       ;; check that the string-link is not nil
       string-link
	   ;; turn the string link into an org element
	   (link (linkin-org-parse-org-link string-link))
	   ;; get the type of the link
	   (link-type (org-element-property :type link))
	   ;; change the string link into a correct link following id, only if its type is in linkin-org-link-types-to-check-for-id
	   (new-string-link (if (member link-type linkin-org-link-types-to-check-for-id)
				            (linkin-org-resolve-link string-link)
                          string-link
			              )
			            )
       )
      ;; open the resolved link in the normal org way
      (org-link-open (linkin-org-parse-org-link new-string-link))
    ;; if the link could not be resolved, just open the link in the normal org way
    (org-link-open string-link)
    )
  )



;;;; ------------------------------------------- file link

;; To create a link towards the file under point in a dired buffer
(defun linkin-org-dired-get-link ()
  "Returns a link towards the file under point in dired"
  (let* (
         (chemin-fichier (abbreviate-file-name (dired-file-name-at-point)))
	 ;; le nom du fichier sans le chemin
	 (nom-fichier
	  ;; if the file under point is a directory
	  (if (file-directory-p chemin-fichier)
	      ;; remove the trailing slash, get the name of the directory
	      (file-name-nondirectory (directory-file-name chemin-fichier))
	    ;; else if it's a file
	   (file-name-nondirectory chemin-fichier)
	   )
	  )
	 ;; le nom du fichier sans l'id, si il y a id
	 (nom-fichier (linkin-org-strip-off-id-from-file-name nom-fichier))
	 (nom-fichier-sans-ext (file-name-sans-extension nom-fichier))
	 (extension (file-name-extension nom-fichier))
	 ;; tronque le nom du fichier s'il est trop long
	 (nom-fichier (if (> (length nom-fichier) 70)
                          (concat (substring nom-fichier-sans-ext 0 50)  " [___] " "." extension)
			nom-fichier)
		      )
	 )
    (if nom-fichier
	;; if it's a file, not a directory
	(kill-new (format "[[file:%s][[file] %s]]" chemin-fichier nom-fichier)))
    ;; otherwise, remove the trailing slash
    (let* (
	   (directory-name-without-slash (directory-file-name chemin-fichier))
	   )
      (format "[[file:%s][[file] %s]]" directory-name-without-slash nom-fichier)
    )
  )
  )

(defun linkin-org-file-open (link)
  "Open the file at LINK."
  (let* (
	 (link-parts (split-string link "::"))
	 (file-path (car link-parts))
     (metadata (when (cadr link-parts)
                 (read (cadr link-parts))
                 )
               )
	 (line-number-or-id (if (plistp metadata)
                            (prin1-to-string (plist-get metadata :inline-id))
                          (cadr link-parts)
                            )
                        )
	 (column-number (if (plistp metadata)
                        (prin1-to-string (plist-get metadata :column))
                      (caddr link-parts)
                      )
                    )
	 ;; (line-number-or-id (cadr link-parts))
	 ;; (column-number (caddr link-parts))
	 )
    (if (file-exists-p file-path)
	    (progn
	      (linkin-org-perform-function-as-if-in-dired-buffer file-path 'dired-open-file)
	      (when line-number-or-id
	        ;; if line-number-or-id matches an id, search for that id in the buffer
	        (let
		        (
		         id-position
		         )
	          (if (string-match linkin-org-id-regexp line-number-or-id)
		          (progn
		            (save-excursion (progn
				                      (beginning-of-buffer)
				                      (setq id-position (if (re-search-forward (concat "id:" line-number-or-id) nil t 1) (point) nil))
				                      )
				                    )
		            (when id-position
		              (goto-char id-position)
		              )
		            )
	            (goto-line (string-to-number line-number-or-id))
	            )
	          )
	        (when column-number
	          (move-to-column (string-to-number column-number))
	          )
	        )
	      )
      )
    )
  )

;; rewriting org-link-open since they do some special treatment for file type links
(defun org-link-open (link &optional arg)
  "Open a link object LINK.

ARG is an optional prefix argument.  Some link types may handle
it.  For example, it determines what application to run when
opening a \"file\" link.

Functions responsible for opening the link are either hard-coded
for internal and \"file\" links, or stored as a parameter in
`org-link-parameters', which see."
  
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (pcase type
      ;; Opening a "file" link requires special treatment since we
      ;; first need to integrate search option, if any.
      ("file"
       (let* (
	      (option (org-element-property :search-option link))
	      (path (if option (concat path "::" option) path))
	      )


	 ;; Start of changes
	 ;; (org-link-open-as-file path
	 ;; 			(pcase (org-element-property :application link)
	 ;; 			  ((guard arg) arg)
	 ;; 			  ("emacs" 'emacs)
	 ;; 			  ("sys" 'system)))))
	 ;; (linkin-org-open-file-as-in-dired path)
	     (linkin-org-file-open path)
	 )
       )
      ;; End of changes

      ;; Internal links.
      ((or "coderef" "custom-id" "fuzzy" "radio")
       (unless (run-hook-with-args-until-success 'org-open-link-functions path)
	 (if (not arg) (org-mark-ring-push)
	   (switch-to-buffer-other-window (org-link--buffer-for-internals)))
	 (let ((destination
		(org-with-wide-buffer
		 (if (equal type "radio")
		     (org-link--search-radio-target path)
		   (org-link-search
		    (pcase type
		      ("custom-id" (concat "#" path))
		      ("coderef" (format "(%s)" path))
		      (_ path))
		    ;; Prevent fuzzy links from matching themselves.
		    (and (equal type "fuzzy")
			 (+ 2 (org-element-begin link)))))
		 (point))))
	   (unless (and (<= (point-min) destination)
			(>= (point-max) destination))
	     (widen))
	   (goto-char destination))))
      (_
       ;; Look for a dedicated "follow" function in custom links.
       (let ((f (org-link-get-parameter type :follow)))
	 (when (functionp f)
	   ;; Function defined in `:follow' parameter may use a single
	   ;; argument, as it was mandatory before Org 9.4.  This is
	   ;; deprecated, but support it for now.
	   (condition-case nil
	       (funcall f path arg)
	     (wrong-number-of-arguments
	      (funcall f path)))))))))


;; to get a link towards the current line in an editale file.
;; if there is an id in the current line, use it. Otherwise use the line number.
(defun linkin-org-get-inline ()
  "Returns a link towards the current line in an editable file.
If there is an inline id in the current line, use it. Otherwise use the line number.
"
  (let*
      (
       (current-file-path (when (buffer-file-name) (expand-file-name (buffer-file-name))))
       (file-name (when current-file-path (file-name-nondirectory current-file-path)))
       ;; get the current line in string
       (current-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)
                      )
                     )
       ;; get the id in the current line, if there is one
       (inline-id (linkin-org-get-id current-line linkin-org-inline-id-regexp))
       ;; remove the leading id: part of the inline id
       (inline-id (replace-regexp-in-string "id:" "" inline-id))
       (line-number (line-number-at-pos))
       )
    ;; get an id only if the file has a path
    (if file-name
     (if inline-id
         (format "[[file:%s::(:inline-id %s)][[file] %s]]"
	             current-file-path
                 inline-id
	             (linkin-org-strip-off-id-from-file-name file-name)
                 )
       (format "[[file:%s::%d][[file] %s _ l%d]]"
		       current-file-path
		       line-number
	           (linkin-org-strip-off-id-from-file-name file-name)
		       line-number
		       )
       )
     ;; else if the file has no path, do nothing
     (message "linkin-org: this file has no path, cannot get an id for it.")
     )
    )
  )


;; to leave an id in an editable line
(defun linkin-org-store-inline ()
  "Leaves an id in an editable line and copy a link towards that id in the kill-ring"
  (let (
	    (id (linkin-org-create-id))
	    (range
         (list (line-beginning-position)
		       (goto-char (line-end-position 1))
		       )
         )
	    (current-line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)
                       )
                      )
	    )
    ;; insert an inline id only if there is none already
    (when (not (linkin-org-get-id current-line))
      ;; quick fix for org-mode
      (if (and
	       (not (eq major-mode 'org-mode))
	       (comment-only-p (apply #'min range) (apply #'max range))
	       )
	      (progn
	        ;; go to the beginning of the commented text
	        (comment-beginning)
	        (insert (concat "[id:" id "] "))
	        )
        ;; else, insert at the beginning of line and comment the line
        (progn
          (back-to-indentation)
          (insert (concat "[id:" id "] "))
          (comment-region (line-beginning-position) (line-end-position))
          ;; (comment-or-uncomment-region
          ;; 	(apply #'min range)
          ;; 	(apply #'max range)
          ;; 	)
          )
        )
      )
    ;; copy the link
    (linkin-org-get)
    ;; (let*
	;;     (
	;;      (file-path (buffer-file-name))
	;;      (file-name (when file-path
	;;                   (file-name-nondirectory file-path)
	;;                   )
	;;                 )
	;;      )
    ;;   ;; kill a link only if the current buffer has a valid file path
    ;;   (if file-path
    ;;       (kill-new (format
	;;                  "[[file:%s::(:inline-id %s)][[file] %s]]"
	;;                  file-path
	;;                  id
	;;                  (linkin-org-strip-off-id-from-file-name file-name)
	;;                  )
	;;                 )
    ;;     (message "Current buffer is not attached to a file, no link was created.")
    ;;     )
    ;;   )
    ;; go to the end of line
    (end-of-line)
    )
  )

;;;; ------------------------------------------- pdf link

(require 'pdf-tools)

(org-add-link-type "pdf" 'org-pdf-open nil)

(defun org-pdf-open (link)
  (let*
      (
	   (link-parts (split-string link "::"))
	   (file-path (car link-parts))
       (metadata (when (cadr link-parts)
                   (read (cadr link-parts))
                   )
                 )
       (pdf-file (car link-parts))
	   (page
        (if (and (plistp metadata) (= 2 (length link-parts)))
            ;; if the link is with the plist format
            (plist-get metadata :page)
          ;; else if the data is just separated by ::
          (string-to-number (car (cdr link-parts)))
          )
        )
       
	   (edges-list
        (if (and (plistp metadata) (= 2 (length link-parts)))
            ;; if the link is with the new plist format
            (let*
	            ((edges-list-str (split-string (plist-get metadata :edges) ";")))
	          ;; convert from string to int, get a list of four floating points numbers, that's the edges
	          (unless (not edges-list-str) (list (mapcar 'string-to-number edges-list-str)))
             )
          ;; else if the data is just separated by ::
	      (let*
              (
               (edges-str (car (cdr (cdr link-parts))))
	           ;; separate the edges by |
	           (edges-list-str (unless (not edges-str) (split-string edges-str "[;|]")))
               )
	        ;; convert from string to int, get a list of four floating points numbers, that's the edges
	        (unless (not edges-list-str) (list (mapcar 'string-to-number edges-list-str)))
            )
          )
        )
	   ;; (path+page+edges (split-string link "::"))
	 ;; ;; for the pdf file path
     ;;     (pdf-file (car path+page+edges))

	 )
    ;; (start-process "view-pdf" nil "zathura" pdf-file (format "--page=%s" page))))
    (progn
      ;; (message edges-list)
      ;; check if the pdf file is already open 
      (if-let (
	       ;; get the buffer of the pdf file
	       (pdf-buffer (get-file-buffer pdf-file))

	       ;; check if the buffer is visible
	       (pdf-window (get-buffer-window pdf-buffer 'visible))

               ;; Trouve toutes les fenêtres qui affichent le buffer donné en entrée
               (windows (delq (selected-window)
                              (get-buffer-window-list
                               pdf-buffer 'nomini t)))
	       
               ;; la première de ces fenêtres
               (fenetre-finale (car windows))
	       
               ;; On initialise le temps le plus récent avec la temps de la première fenêtre de la liste
               (temps-le-plus-recent (window-use-time fenetre-finale))

	       ;; (not linkin-org-open-org-link-other-frame)
	       )
	       ;; (pdf-buffer (get-file-buffer pdf-file))
	       ;; ;; check if the buffer is visible
	       ;; (pdf-window (get-buffer-window pdf-buffer 'visible))
			  ;; )
	      ;; then dont open a new frame, rather switch to the last visisted window and highlight the edges
	      (progn


                   ;; Parmi celles qui affichent le buffer, séléctionne la fenêtre la plus récement utilisée
                   (dolist (fenetre (cdr windows))
                     (if (> (window-use-time fenetre) temps-le-plus-recent)
                         (progn
                           (setq temps-le-plus-recent (window-use-time fenetre))
                           (setq pdf-window fenetre)
                           )
                       )
                     )

		   (let (
			 (initial-window (selected-window))
			 )
		     ;; switch to the frame
		     ;; (select-frame-set-input-focus (window-frame pdf-window) t)
		    ;; (select-frame (window-frame pdf-window))
		    ;; switch to the window
		    (select-window pdf-window)

		    ;; (if edges-list
		    ;;     ;; if the place to highlight is provided, then make sure that place is visible (ie, pdf is scrolled so that one can see it)
		    ;;     (let
		    ;; 	;; idk then do the same here
		    ;; 	;; [[file:~/.config/emacs/straight/repos/pdf-tools/lisp/pdf-occur.el::290][[file] pdf-occur.el_at_290]]
		    ;; 	(
		    ;; 	 (pdf-isearch-batch-mode t)
		    ;; 	 (pixel-match (pdf-util-scale-relative-to-pixel edges-list))
		    ;; 	 )
		    ;;       (pdf-isearch-focus-match-batch pixel-match)
		    ;;     
		    ;;       (pdf-isearch-hl-matches pixel-match nil t)
		    ;;       )
		    ;;   ;; else, just go to the page
		    ;;   (unless (not page) 
		    ;;     (pdf-view-goto-page page)
		    ;;     )
		    ;;   )


		    ;; go to the page
		    (unless (not page) 
		      (pdf-view-goto-page page)
		      )
		    ;; ;; hightlight the edges
		    (unless (not edges-list)
		      (let
			  ;; idk then do the same here
			  ;; [[file:~/.config/emacs/straight/repos/pdf-tools/lisp/pdf-occur.el::290][[file] pdf-occur.el_at_290]]
			  (
			   (pdf-isearch-batch-mode t)
			   (pixel-match (pdf-util-scale-relative-to-pixel edges-list))
			   )
			;; dont forget to scale the edges to the current display!
			(pdf-isearch-hl-matches pixel-match nil t)
			(pdf-isearch-focus-match-batch pixel-match)
			)
		      )
		    ;; switch back to the initial buffer
		    (select-window initial-window)
		    )
		   )
	;; if the pdf file is not visible, open a new frame
	;; or if I specifically asked to open a new frame for the pdf file
	(progn
	  ;; (message "not visible")
	  (clone-frame)
	  (find-file pdf-file)
	  (pdf-view-goto-page page)
	  )
	;; (if linkin-org-open-pdf-link-other-frame
	;; 	  (progn
	;; 	    (find-file-other-frame pdf-file)
	;; 	    (pdf-view-goto-page page)
	;; 	    )
	;; 	)
	)
      )
    )
  )

;; pour copier un lien vers le fichier pdf courant
(defun linkin-org-pdf-get-link ()
  (interactive)
  (other-window 1)
  (pdf-tools-assert-pdf-buffer)
  (let* (
	 (page (number-to-string (pdf-view-current-page)))
         (file (abbreviate-file-name (pdf-view-buffer-file-name)))
	 (file-name (file-name-nondirectory file))
	 (file-name-sans-id (linkin-org-strip-off-id-from-file-name file-name))
	 (file-name-sans-ext (file-name-sans-extension file-name-sans-id))
	 (file-name-extension (file-name-extension file-name-sans-id))
	 (nom-fichier-tronque (if (> (length file-name-sans-ext) 70)
				  (concat (substring file-name-sans-ext 0 50)
					  "[___]"
					  (if file-name-extension
					      (concat "." file-name-extension)
					    )
					  )
				file-name-sans-id
				)
			      )
	 ;; for selected text
	 (selected-text (if (pdf-view-active-region-p)
			    ;; delete the newlines
			    (replace-regexp-in-string "\n" " " (car (pdf-view-active-region-text)))
			  nil
			  )
			)

	 ;; ;; truncate the selected text
	 ;; (selected-text (if (and selected-text (> (length selected-text) 15))
	 ;; 			     (concat (substring selected-text 0 15) "...")
	 ;; 			   selected-text
	 ;; 			   )
	 ;; 			 )

	 ;; for the edges
	 (edges (if (pdf-view-active-region-p)
                ;; the edges that were really hovered by the mouse.
                ;; not necessarily the visual highlightings, that are wordwise by default
                ;; (pdf-info-getselection
                ;;  (string-to-number page)
                ;;  pdf-view-active-region
                ;;  pdf-view-selection-style
                ;;  )
              
                (mapcar
                 (lambda (edges)
                   (pdf-info-getselection
                    (string-to-number page)
                    edges
                    pdf-view-selection-style)
                   )
                 pdf-view-active-region)
		      nil
		      )
	        )
	 )
    (other-window 1)

    ;; deselect the text, if there is an active region
    (if (pdf-view-active-region-p)
	(pdf-view-deactivate-region)
      )

    (if (and selected-text edges)
	(progn
	 (let*
	     (
	      ;; edges are actually outputed as a list of list of Lists-trees
	      (edges (car (car edges)))
	      ;; concat the edges with |
	      (string-edges (concat "\"" (mapconcat #'prin1-to-string edges ";") "\"" ))
	      )

	   ;; (format "[[pdf:%s::%s::%s][[pdf] %s _ p%s _ \"%s\"]]" file page string-edges nom-fichier-tronque page selected-text)
	   ;; without the pdf name
	   ;; (format "[[pdf:%s::%s::%s][[pdf] p%s _ \"%s\"]]" file page string-edges page selected-text)
	   (format "[[pdf:%s::(:page %s :edges %s)][[pdf] p%s _ \"%s\"]]" file page string-edges page selected-text)
	   )
	 )
      ;; else, no selected text, just care about the path and page
      ;; (format "[[pdf:%s::%s][[pdf] %s _ p%s]]" file page nom-fichier-tronque page)
      (format "[[pdf:%s::(:page %s)][[pdf] %s _ p%s]]" file page nom-fichier-tronque page)
      )
    )
  )



;;;; ------------------------------------------- music link
(org-add-link-type "mpd" 'org-mpd-open nil)
;; ishould use this instead
;; (org-link-set-parameters TYPE &rest PARAMETERS)


(defun org-mpd-open (string-link)
  " link is a string containing
the paths to the song (an mp3 file or so, or a .cue file with a trailing /track<number>) as a lisp list, each song is a string element of the list
then ::,then, a timestamp in format readable by mpd, for instance 1:23:45 "
  (let* (
	     (link-parts (split-string string-link "::"))
	     (songs (read (car link-parts)))
         (metadata (when (cadr link-parts)
                     (read (cadr link-parts))
                     )
                   )
	     ;; unescape the link
	     ;; (link (unescape-special-characters link))
	     ;; use the read function that parses a string as code
	     ;; (songs (read (car link-parts)))
	     (timestamp
          (if (and (plistp metadata) (= 2 (length link-parts)))
              ;; if the metadata are in the plist format
              (plist-get metadata :timestamp)
            ;; else if the data is just separated by ::
            (cadr link-parts)
            )
	      )
         )
    ;; (message (concat "song:" (prin1-to-string songs)))
    ;; (simple-mpc-call-mpc nil (cons "add" songs))
    (apply 'call-process "mpc" nil nil nil (cons "add" songs))
    )
  )

;; code that takes a mpd entry list (with file, title, etc) and returns the title
(defun linkin-org-get-mpd-track-title (lst)
  "Return the element after 'Title if present in LST, else the element after 'file."
  (let ((title-pos (cl-position 'Title lst))
        (file-pos (cl-position 'file lst)))
    (cond
     ;; if there is a 'Title elem in the list and if there is a value (a next elem after 'Title)
     ((and title-pos (not (null (nth (1+ title-pos) lst))))
      (nth (1+ title-pos) lst)) ;; Return the element after 'Title
     ;; else, if there is a 'file elem in the list and if there is a value (a next elem after 'file)
     ((and file-pos (not (null (nth (1+ file-pos) lst))))
      (let*
	  (
	   (file-path (nth (1+ file-pos) lst))
	   ;; Get the file name from the file path
	   (file-name (file-name-nondirectory file-path))
	   ;; get the file name without the extension
	   (file-name (file-name-sans-extension file-name))
	   ;; shorten the file name if it is too long
	   (max-length 50)
	   (file-name (if (> (length file-name) max-length)
			  ;; If file-name is longer than 15 characters, truncate it
			  (concat (substring file-name 0 max-length) "[___]")
			file-name
			)
		      )
	   )
	file-name
	)
      )
     ;; Return nil if neither found
     (t nil)
     )
    )
  )


;; returns a link in string format towards the mingus entry at point
(defun linkin-org-lien-mpd-mingus ()
  "Returns a link in string format towards the mingus entry at point"
  (let* (
	 (list-songs
	  (mapcar
	   (lambda (index)
	     ;; the song normally is the second element
	     (nth 1 (car (mpd-get-playlist-entry mpd-inter-conn index nil t)))
	     )
	   mingus-marked-list
	   )
	  )
	 ;; remove any nil element
	 (list-songs (remove nil list-songs))
	 ;; reverse the list
	 (list-songs (reverse list-songs))
        )
    ;; if there are marked songs
    (if list-songs
	(let
	    (
	     ;; get the file name of the first song
	     (title (if list-songs
			(linkin-org-get-mpd-track-title (car
						 (mpd-get-playlist-entry
						  mpd-inter-conn
						  (car (last mingus-marked-list))
						  nil
						  t
						  )
						 )
						))
		    )
	     )
	 (format
	  ;; "[[mpd:%s::00:00:00][[music] %s _ 00:00]]"
	  "[[mpd:%s::00:00:00][[music] %s]]"
	  (linkin-org-transform-square-brackets (prin1-to-string list-songs))
	  title
	  )
	 )
      ;; else
      (let (
	    (track-path (nth 1 (mingus-get-details)))
	    (title (linkin-org-get-mpd-track-title (mingus-get-details)))
	    )
	;; (format "[[mpd:(\"%s\")::00:00:00][[music] %s _ 00:00]]" track-path title)
	(format "[[mpd:(\"%s\")::(:timestamp \"00:00:00\")][[music] %s]]" (linkin-org-transform-square-brackets track-path) title)
	)
     )
    )
  )

(defun linkin-org-link-mpd-simple-mpc ()
  (interactive)
  (let*
      (
       (track-path
	(simple-mpc-query-get-%file%-for-result
	 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	)
	
       ;; remove the folder part and the extension
       (title
	(file-name-nondirectory (file-name-sans-extension track-path))
	)
       )
    (format
     "[[mpd:(\"%s\")::00:00:00][[music] %s]]"
     track-path
     title
     )
    )
  )

;;;; ------------------------------------------- video link
(org-add-link-type "video" 'org-video-open nil)

(defun linkin-org-is-url (string)
  "Return non-nil if STRING is a valid URL."
  (string-match-p
   (rx string-start
       (seq (or "http" "https" "ftp") "://")
       (1+ (not (any " ")))  ;; Match one or more non-space characters
       string-end)
   string))

(defun org-video-link-get-path (link)
  (let* (
         (path+timestamp (split-string link "::"))
         ;; for the video file path
         (video-file (car path+timestamp))
         )
    video-file
    )
 ) 

(defun org-video-open (link)
  "Where timestamp is 00:15:37.366 , the link should look like:
   [[video:/path/to/file.mp4::00:15:37.366][My description.]]
   path can also be a youtube url
   "

  (let* (
	 (path-or-url+timestamp (split-string link "::"))
         (timestamp (car (cdr path-or-url+timestamp)))
         (video-address (car path-or-url+timestamp))
	 )
    (progn
      ;; (message (concat "video address : " video-address))
      (cond
       ;; if its an url
       ((linkin-org-is-url video-address)
	    (progn
	      (start-process "mpv" nil "mpv" "--force-window" "--ytdl=no" video-address)
	      )
	    )
       ;; if it's a local file
       ;; ((linkin-org-is-link-path-correct video-address)
       ((linkin-org-resolve-file video-address)
	    (progn
	      (message (format  "Playing %s at %s" video-address timestamp))
	      (start-process "mpv" nil "mpv" (format "--start=%s" timestamp) video-address)))
       (t
	    (message "Not a valid video file or url")
	    )
       
       )
      )
    )
  )


;;;; ------------------------------------------- saving mu4e attachment

(defun linkin-org-store-mu4e-attachment ()
  (let* (
	 ;; get text at point
	 (data (get-text-property (point) 'gnus-data))
	 ;; get name of the attachment
	 (filename-without-directory (or (mail-content-type-get
					  (mm-handle-disposition data) 'filename)
					 (mail-content-type-get
					  (mm-handle-type data) 'name)))

	 (nouveau-filename-without-directory (read-string "Nom : " (file-name-nondirectory filename-without-directory)))
	 (id (linkin-org-create-id))
	 (nouveau-filename-without-directory (concat id linkin-org-sep nouveau-filename-without-directory))
	 (nouveau-filename (concat linkin-org-store-directory "/" nouveau-filename-without-directory))
	 )
    (mm-save-part-to-file data nouveau-filename)
    ;; yank link to file
    (linkin-org-yank-link-of-file nouveau-filename)
    )
  )


;;;; ------------------------------------------- main commands

(defun linkin-org-leave-id-to-file-in-dired ()
  "add an id to the under point in dired.
Do nothing if the file already has an id.
"
    (interactive)
    (let* (
	       (file-path (dired-file-name-at-point))
           ;; t if file-path is a directory, nil if it's a file
           (is-directory? (file-directory-p file-path))
           ;; get the file-name, or directory name
           (file-name
            (if is-directory?
                (file-name-nondirectory (directory-file-name file-path))
              (file-name-nondirectory file-path)
                )
            )
           ;; get the file path without the name of the file
           (file-path-sans-name
            (if is-directory?
                (file-name-directory (directory-file-name file-path))
              (file-name-directory file-path)
              )
            )
           )
      ;; if the file doesnt already has an id, rename the file or directory with an id at the front
      (unless (linkin-org-get-id file-name)
        (rename-file
         file-path
         (concat
          file-path-sans-name
          (linkin-org-give-id-to-file-name file-name)
          )
         )
        (revert-buffer)
        )
      )
    )


(defun linkin-org-follow ()
  "Open the link under point.
If a region is selected, open all links in that region in order.
"
  (interactive)
  ;; if a region is selected, then open all links in the region, in order
  (if (region-active-p)
      (let (
	    (beg (region-beginning))
	    (end (region-end))
	    (text-to-investigate (buffer-substring-no-properties (region-beginning) (region-end)))
	    )
	    (save-excursion
	      (with-temp-buffer
	        ;; insert the text to investigate
	        (insert "\n")
	        (insert text-to-investigate)
	        ;; go to the beginning of the buffer
	        (goto-char (point-min))
	        ;; if there is a link under point
	        ;; (if (org--link-at-point)
	        ;; 	;; open the link
	        ;; 	(linkin-org-open-link-at-point)
	        ;; 	)
	        (let*
		        (
		         ;;remember the current point
		         (current-point (point))
		         ;; go to the next link and remember the point
		         (next-point (progn
			                   (org-next-link)
			                   (point))
			                 )
		         )
	          ;; go to the next link while current-point is different from next-point
	          (while (not (= current-point next-point))
		        (linkin-org-follow)
		        (setq current-point next-point)
		        (setq next-point (progn
				                   (org-next-link)
				                   (point))
		              )
		        )
	          )
	        )
	      )
	    )
    (let (
	      ;; get the link under point in string form
	      (string-link (linkin-org-get-org-string-link-under-point))
	      )
      ;; follow the string
      (linkin-org-follow-string-link string-link)
      
      )
    )
  )



(defun linkin-org-get ()
  "kill a link to what is under point"
  (interactive)
  (let*
      ((mode (symbol-name major-mode)))
    (cond
     ;; if in a pdf, kill a link towards the pdf
     ((string= mode "pdf-view-mode")
      (kill-new (linkin-org-pdf-get-link))
      )
     ;; if text is selected, just kill the that text
     ((region-active-p)
      (kill-ring-save (region-beginning) (region-end))
      )
     ;; if in a dired buffer, kill a link towards the file under point
     ((string= mode "dired-mode")
      (kill-new (linkin-org-dired-get-link))
      )
     ;; if viewing a mail in mu4e
     ((or (string= mode "mu4e-view-mode") (string= mode "mu4e-headers-mode"))
      ;; (kill-new (take-list-of-two-strings-and-make-link (call-interactively 'org-store-link) "[mail]"))
      (kill-new (let
                 ((l (call-interactively 'org-store-link)))
                 (format "[[%s][%s %s]]"
                        (car l)
                        "[mail]"
                        (cadr l)
                        )
                 )
                )
      )
     ;; if in a mingus playlist buffer
     ((string= mode "mingus-playlist-mode")
      (kill-new (linkin-org-lien-mpd-mingus))
      )
     ;; if in a simple-mpc buffer
     ((string= mode "simple-mpc-mode")
      (kill-new (linkin-org-link-mpd-simple-mpc))
      )


     ;; Otherwise, kill a link towards the current line of the buffer
     (t
      (kill-new (linkin-org-get-inline))
      )

     )
    )
  )


(defun linkin-org-store ()
  "Store what is under point and kill a link to it"
  (interactive)
  (let*
      ((mode (symbol-name major-mode)))
    (cond
     ;; ;; If text is selected
     ;; ((region-active-p)
     ;;  (progn
     ;;   (my-store-some-text
     ;;    (buffer-substring (region-beginning) (region-end))
     ;;    "[[file:/home/juliend/Dropbox/FourreTout/Notes/20240504T120559--fourretout-nimp-liens-todo__doc.org::(:inline-id 20250417T223553)]]"
     ;;    )
     ;;   ;; unselect the region
     ;;   (deactivate-mark)
     ;;   )
     ;;  )
     ;; ;; If in elfeed
     ;; ((string= mode "elfeed-search-mode")
     ;;  (my-save-elfeed-entry "[[file:/home/juliend/Dropbox/FourreTout/Notes/20240504T120559--fourretout-nimp-liens-todo__doc.org::(:inline-id 20250418T001752)][[file] fourretout-nimp-liens-todo__doc.org]]")
     ;;  )
     ;; If in a dired buffer
     ((string= mode "dired-mode")
      (linkin-org-store-file t)
      )
     ;; If in mu4e
     ((string= mode "mu4e-view-mode")
      (linkin-org-store-mu4e-attachment)
      )
     ;; If in an editable buffer
     ((not buffer-read-only)
      (linkin-org-store-inline)
      )
     )
    )
  )



(provide 'linkin-org)
