;;; linkin-org.el --- A workflow with fast, reliable links -*- lexical-binding: t -*-

;; Copyright 2025 Julien Dallot

;; Author: Julien Dallot <judafa@protonmail.com>
;; Maintainer: Julien Dallot <judafa@protonmail.com>
;; URL: https://github.com/Judafa/linkin-org
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (s "1.30.0") (dash "2.20.0"))

;; This file is not part of GNU Emacs

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; linkin-org proposes to access your data with reliable links.
;; The links work fast and are easy to create.
;; Most importantly, the links are reliable and
;; robustly support a whole link-based workflow.


(require 'ol)
(require 'org)
(require 'org-element-ast)
(require 's)
(require 'dash)
(require 'dired)


;;; Code:

;;;; -------------------------------------------- main variables



(defgroup linkin-org nil
  "A package to make org link reliable."
  :group 'convenience)



;; define the directory where the function linkin-org-store stores the files and directories by default
(defcustom linkin-org-store-directory
  (expand-file-name "~/")
  "The directory where `linkin-org-store' stores data by default."
  :type 'string
  :group 'linkin-org)

;; define whether to store the file directly in the directory defined by `linkin-org-store-directory' without asking anything
(defcustom linkin-org-store-file-directly-p
  nil
  "If non-nil, store the file to store directly in `linkin-org-store-directory'."
  :type 'boolean
  :group 'linkin-org)

;; define the directories where to search when a link is broken
;; this is a list of directories that are searched in order to resolve broken links
(defcustom linkin-org-search-directories-to-resolve-broken-links
  (list (expand-file-name "~/"))
  "The list of directories to search (in order) when a link is broken."
  :type '(string)
  :group 'linkin-org)

;; define a regexp to match file names (without the directory part) that are not considered when resolving broken links
(defcustom linkin-org-file-names-to-ignore
  (rx
   (or (seq (* anychar) "~" line-end) (seq line-start "" line-end)))
  "Regexp that matches file names not considered when resolving broken links."
  :type 'regexp
  :group 'linkin-org)

;; List of link types such that, if the link is broken, the ids in the link are used to resolve the link
(defcustom linkin-org-link-types-to-check-for-id
  '("file" "pdf")
  "List of link types such that their path is resolved when opened."
  :type '(string)
  :group 'linkin-org)


(defcustom linkin-org-id-position-in-file-name
  'head
  "The position of the id in the file or directory name, can be head or tail."
  :type 'symbol
  :group 'linkin-org)


(defcustom linkin-org-open-links-as-in-dired-p
  nil
  "If non-nil, open links as if they were opened in Dired."
  :type 'boolean
  :group 'linkin-org)

(defcustom linkin-org-opening-file-function-in-dired
  #'dired-find-file
  "Function to use to open a file from Dired."
  :type 'function
  :group 'linkin-org)



;;;; -------------------------------------------- patterns


;; regexp recognizing an id
(defconst linkin-org-id-regexp
  (rx
   (or
    ;; denote style
    ;; contains the timestamp, and the signature if there is one
    (seq
     (= 4 digit)
     (= 2 digit)
     (= 2 digit)
     "T"
     (= 2 digit)
     (= 2 digit)
     (= 2 digit)
     (? (seq "==" (* alnum))))
    ;; org-roam style
    (seq line-start
	 (= 4 digit)
	 (= 2 digit)
	 (= 2 digit)
	 (= 2 digit)
	 (= 2 digit)
	 (= 2 digit)))))


;; regexp recognizing an inline id.
;; inline ids are ids written directly in the file.
;; their id must be distinctive from file id since we dont want links to be identified as inline ids; just add "id:" in front of it.
(defconst linkin-org-inline-id-regexp
  (concat "id:" linkin-org-id-regexp))




;; regexp recognizing a separator between id and original filename
(defconst linkin-org-sep-regexp (rx (or "--" "-")))

;; separator between the id and the original file name
(defconst linkin-org-sep "--")



;;;; ------------------------------------------- basic functions


(defun linkin-org-create-id ()
  "Return an id.
The id is a string with the year, month, day, hour, minute, second."
  (let* ((time-string
	  (format-time-string "%Y%m%dT%H%M%S" (current-time))))
    time-string))

(defun linkin-org-extract-id (s &optional id-regexp)
  "Return a substring of string S that matches ID-REGEXP.
Returns nil if no match was found.
If ID-REGEXP is not provided then replace it with `linkin-org-id-regexp'."
  (unless id-regexp (setq id-regexp linkin-org-id-regexp))
  (when (stringp s)
    (when (string-match id-regexp s)
      ;; this function returns a list of list of strings
      (car (car (s-match-strings-all id-regexp s))))))

(defun linkin-org-strip-off-id-from-file-name (file-name)
  "Take a file name FILE-NAME (without path) and strip off the id part.
Also strip off the separator -- if possible."
  ;; (let* (
  ;; 	 (id (linkin-org-extract-id file-name linkin-org-id-regexp)))
    ;; (if id
	;; (let* (;; remove the id
	;;        (file-name-without-id
	;; 	(replace-regexp-in-string id "" file-name)))
  (cond
   (
    ;; if the id is at the tail of the file name
    (string-match
     (rx (seq (zero-or-more anychar) (regexp linkin-org-id-regexp) (? (seq "." (zero-or-more (not ".")))) line-end))
     file-name)
    (replace-regexp-in-string
     (rx (seq (? (regexp linkin-org-sep-regexp)) (regexp linkin-org-id-regexp) (? (seq "." (zero-or-more (not ".")))) line-end))
     (lambda (str)
       ;; return the extension, if there is one
       (save-match-data
	 (if (string-match (rx (seq "." (zero-or-more (not ".")) line-end)) str)
	     (match-string 0 str)
	   ""
	   )
	 )
       )
     file-name))
   (
    ;; else if the id is at the head of the file name
    (string-match
     (rx (seq line-start (regexp linkin-org-id-regexp) (zero-or-more anychar)))
     file-name)
    (replace-regexp-in-string
     (rx (seq line-start (regexp linkin-org-id-regexp) (? (regexp linkin-org-sep-regexp))))
     ""
     file-name)
    )
   (t
    ;; else if the id is neither at the beginning or at the end, just remove the id
    (replace-regexp-in-string
     linkin-org-id-regexp
     ""
     file-name))))

(defun linkin-org-give-id-to-file-name (file-name &optional id)
  "Insert an id into FILE-NAME.
If ID is provided, use it as the id.
Does not add an id if FILE-NAME already has one."
  (if (linkin-org-extract-id file-name)
      ;; if the file already has an id, dont add one
      file-name
    ;; else add an id
    (if id
	;; if we must add the id at the tail of the file name
	(if (eq linkin-org-id-position-in-file-name 'tail)
	    (replace-regexp-in-string
	     (rx
	      (seq
	       (? (seq "." (zero-or-more (not "."))))
	       line-end))
	     (lambda (match) (concat linkin-org-sep id match))
	     file-name)
	  ;; else just add the id at the head of the file name
	  (concat id linkin-org-sep file-name))
      ;; else if no id is provided
      (if (eq linkin-org-id-position-in-file-name 'tail)
	  (replace-regexp-in-string
	   (rx
	    (seq
	     (? (seq "." (zero-or-more (not "."))))
	     line-end))
	   (lambda (match)
	     (concat linkin-org-sep (linkin-org-create-id) match))
	   file-name)
	;; else just add the id at the head of the file name
	(concat (linkin-org-create-id) linkin-org-sep file-name)))))

(defun linkin-org-escape-square-brackets (str)
  "Escape occurrences of '\\\\', '\\[', and '\\]' in the string STR."
  (let ((new-string
	 (replace-regexp-in-string
	  "\\[" "\\\\["
	  (replace-regexp-in-string "\\]" "\\\\]" str))))
    new-string))


(defun linkin-org-link-escape (string-link)
  "Escape characters \, [ and ] that appear in STRING-LINK."
  (replace-regexp-in-string
   (rx
    (seq
     (group (zero-or-more "\\"))
     (group (or string-end (any "[]")))))
   (lambda (m)
     (concat
      (match-string 1 m)
      (match-string 1 m)
      (and (/= (match-beginning 2) (match-end 2)) "\\")))
   string-link nil t 1))


(defun linkin-org-package-installed-p (pkg-name)
  "Return t if a package named PKG-NAME is installed."
  (let ((cmd (format "%s --version" pkg-name)))
    (eq (call-process-shell-command cmd) 0)))


(defun linkin-org-resolve-path-with-path (file-path)
  "Return a correct file path by resolving FILE-PATH with id.
Return nil if no such path could be found.
Starting from the root directory, climbs up FILE-PATH directory by directory;
Whenever the current subpath is not valid, tries resolving using id.
This finds your file back if you only renamed files (and preserved the ids).
It does not work if you changed the location of some directories in FILE-PATH.
It is assumed you already checked that FILE-PATH is not a valid path."
  (let* (;; expand file path
	 (file-path (expand-file-name file-path))
	 ;; the directory in construction
	 (building-dir "/")
	 ;; split the dir into all its intermediary directories
	 ;; doesnt work on windows!
	 (split-path (split-string file-path "/"))
	 ;; remove empty strings ""
	 (split-path (seq-remove 'string-empty-p split-path)))
    (dolist (sub-dir split-path)
      ;; building-dir is set of nil as soon as we know the path cannot be resolved.
      (when building-dir
	(let ((tmp-building-dir
	       (concat
		(file-name-as-directory
		 (directory-file-name building-dir))
		sub-dir))
	      resolved-dir)
	  (if (file-exists-p tmp-building-dir)
	      ;; if the subdir is already valid, just pile it up
	      (setq building-dir tmp-building-dir)
	    ;; else, try to resolve it with id
	    (if-let* ;; get the id of the considered file/dir, if it exists
		((id
		  (linkin-org-extract-id sub-dir linkin-org-id-regexp)))
		;; if the file has an id, try to resolve it
		(cond
		 (;; try with fd, if installed
		  (linkin-org-package-installed-p "fd")
		  (with-temp-buffer
		    (let ((found?
			   (call-process "fd" nil
					 (current-buffer)
					 nil
					 (format "--base-directory=%s" building-dir)
					 ;; just search by breadth in the current directory, not recursively
					 "--max-depth=1"
					 id)))
		      ;; collect the results
		      (when (and
			     (eq found? 0)
			     (not (zerop (buffer-size))))
			(setq resolved-dir
			      ;; collect all the results
			      (string-lines (buffer-string))))))
		  ;; remove all the strings that should be ignored
		  (setq resolved-dir
			(seq-filter
			 (lambda (s)
			   (not (string-match-p linkin-org-file-names-to-ignore s)))
			 resolved-dir))
		  ;; take the first match, after filtering
		  (setq resolved-dir (car resolved-dir))
		  (if resolved-dir
		      ;; if we found a match, append the resolved dir to the building dir
		      (setq building-dir
			    (if (file-exists-p resolved-dir)
				;; I do this since I'm never sure whether fd returns an absolute or relative path
				resolved-dir
			      (concat
			       (file-name-as-directory
				(directory-file-name building-dir))
			       resolved-dir)))
		    ;; if we did not find a match, then we cannot resolve the file. set the buidling path to nil
		    (setq building-dir nil)))
		 (t
		  ;; else, just use emacs-lisp code to find a matching id. slowest option
		  (setq building-dir
			(car
			 (-filter
			  (lambda (s)
			    (or
			     (not (s-equals? s "."))
			     (not (s-equals? s ".."))))
			  (directory-files building-dir t id t))))))
	      ;; else if there is no id, then we cannot resolve the file. set the buidling path to nil
	      (setq building-dir nil))))))
    building-dir))


(defun linkin-org-resolve-path-with-store-directory (file-path &optional directories-to-look-into)
  "Return a correct file path by resolving FILE-PATH with ids.
It recursively searches for files contained in DIRECTORIES-TO-LOOK-INTO.
Returns nil if FILE-PATH has no id or if no matching file was found.
FILE-PATH can be the path of a file or a directory.
If not provided, DIRECTORIES-TO-LOOK-INTO
is set to `linkin-org-search-directories-to-resolve-broken-links'.
It is assumed you already checked that FILE-PATH is not a valid path before."
  (let* (;; expand file path
	 (file-path (expand-file-name file-path))
	 ;; set the list of directories to look into to the default if it was not provided
	 (directories-to-look-into
	  (if directories-to-look-into
	      directories-to-look-into
	    linkin-org-search-directories-to-resolve-broken-links))
	 ;; get the name and id of the file or directory to look for
	 (file-name
	  (if (file-directory-p file-path)
	      ;; if the file path is that of a directory
	      (file-name-nondirectory (directory-file-name file-path))
	    ;; else if the file path is a file
	    (file-name-nondirectory file-path)))
	 (id (linkin-org-extract-id file-name))
	 (file-found-p
	  (if directories-to-look-into 'search-in-progress 'not-found))
	 (tmp-dirs directories-to-look-into)
	 resolved-file-path)
    ;; try for each dir in directories-to-look-into
    (while (and id (eq file-found-p 'search-in-progress))
      ;; take one directory in the directories to look into
      (let* ((dir (expand-file-name (car tmp-dirs))))
	;; consumes the head directory in tmp-dirs
	(setq tmp-dirs (cdr tmp-dirs))
	;; check if dir is a valid directory.
	;; if not, try to resolve it with ids
	(unless (file-exists-p dir)
	  (setq dir (linkin-org-resolve-path-with-path dir)))
	;; if dir exists or could be resolved
	(when dir
	  (cond
	   (;; try with fd, if installed
	    (linkin-org-package-installed-p "fd")
	    (with-temp-buffer
	      (let ((found?
		     (call-process "fd" nil
				   (current-buffer)
				   nil
				   (format "--base-directory=%s" dir)
				   id)))
		;; collect the results
		(when (and
		       (eq found? 0)
		       (not (zerop (buffer-size))))
		  (setq resolved-file-path
			;; collect all the results
			(string-lines (buffer-string)))
		  ;; remove all the strings that should be ignored
		  (setq resolved-file-path
			(seq-filter
			 (lambda (s)
			   (not (string-match-p linkin-org-file-names-to-ignore s)))
			 resolved-file-path))
		  ;; take the first match, after filtering
		  (setq resolved-file-path (car resolved-file-path)))))
	    ;; if we found a match, the search is over
	    (when resolved-file-path
	      (setq file-found-p 'found)
	      ;; I do this since I'm never sure whether fd returns an absolute or relative path
	      (setq resolved-file-path
		    (if (file-exists-p resolved-file-path)
			resolved-file-path
		      (concat
		       (file-name-as-directory
			(directory-file-name dir))
		       resolved-file-path))))
	    ;; if we found no match and if we looked into all the dirs
	    (when (and (not resolved-file-path) (not tmp-dirs))
	      (setq file-found-p 'not-found)))
	   (t
	    (setq resolved-file-path
		  (car
		   (-filter
		    ;; get rid of the stupid "." and ".." files
		    (lambda (s)
		      (or
		       (not (s-equals? s "."))
		       (not (s-equals? s ".."))))
		    (directory-files-recursively dir id t))))
	    ;; if we found a match, the search is over
	    (when resolved-file-path (setq file-found-p 'found))
	    ;; if we found no match and if we looked into all the dirs
	    (when (and (not resolved-file-path) (not tmp-dirs))
	      (setq file-found-p 'not-found)))))))
    ;; return the resolved path if it was found, else return nil
    (unless (eq file-found-p 'not-found) resolved-file-path)))


(defun linkin-org-resolve-path (file-path)
  "Try different approaches to resolve the file paht FILE-PATH."
  (cond
   ;; if the path is already correct, do nothing
   ((file-exists-p file-path)
    file-path)
   ;; else, try resolving the file path with just ids
   ((linkin-org-resolve-path-with-path file-path))
   ;; else, try resolving the file path looking inside store directories
   ((linkin-org-resolve-path-with-store-directory file-path))
   ;; else, file could not be resolved
   (t (message "Neither the file nor the id could be found"))))



;; to do an action on a file as if the point was on that file in Dired
(defun linkin-org-perform-function-as-if-in-dired-buffer (file-path function-to-perform)
  "Apply FUNCTION-TO-PERFORM on a file with path FILE-PATH.
The function is applied as if the point was on that file in Dired."
  (let* (;; to make operation silent
	 (org-inhibit-startup nil)
	 ;; get the full path
	 (file-path (expand-file-name file-path))
	 ;; if file-path is the path of a directory, make sure there is a trailing slash
	 (is-directory? (file-directory-p file-path))
	 (file-path
	  (if (and is-directory? (not (string-empty-p file-path)))
	      (directory-file-name file-path)
	    file-path))
	 ;; get the list of Dired buffers that already visit the directory of the file
	 (dired-buffers-visiting-path
	  (dired-buffers-for-dir (file-name-directory file-path)))
	 ;; create a Dired buffer visiting the directory of the file (or get the name of it if it already exists)
	 (dired-buffer
	  (dired-noselect (file-name-directory file-path)))
	 ;; clone the Dired buffer
	 (cloned-dired-buffer
	  (with-current-buffer dired-buffer (clone-buffer))))
    ;; switch to the cloned Dired buffer
    (switch-to-buffer cloned-dired-buffer)
    ;; update the cloned Dired buffer
    (revert-buffer)
    ;; place the point on the file
    (dired-goto-file file-path)
    ;; do the function
    (funcall function-to-perform)
    ;; kill the cloned Dired buffer
    (kill-buffer cloned-dired-buffer)
    ;; close the Dired buffer if it was open in the first place
    (unless dired-buffers-visiting-path (kill-buffer dired-buffer))))


(defun linkin-org-store-file ()
  "Store the file under point in Dired."
  (let* ((file-path (dired-file-name-at-point))
	 (file-name
	  (cond
	   ;; if it's a directory
	   ((file-directory-p file-path)
	    (file-name-nondirectory (directory-file-name file-path)))
	   ;; else if it's a file
	   (t (file-name-nondirectory file-path))))
	 ;; is the file already in the list of directories to check in case of a broken link
	 (is-file-already-in-store-directory?
	  (cl-some #'identity
		   (mapcar
		    (lambda (dir)
		      (s-prefix?
		       (expand-file-name dir)
		       (expand-file-name file-path)))
		    linkin-org-search-directories-to-resolve-broken-links))))
    ;; check wether it's a file or a directory
    (if (file-directory-p file-path)
	;; if it's a directory
	(progn
	  (let* ;; compute the complete new file path to store the dir into, without id, including name at the end
	      ((new-raw-file-path
		(cond
		 ((not linkin-org-store-file-directly-p)
		  (let ((raw-user-given-path
			 (read-directory-name
			  "Give new location: "
			  (file-name-as-directory linkin-org-store-directory)
			  (file-name-as-directory linkin-org-store-directory)
			  nil)))
		    (if (file-directory-p raw-user-given-path)
			;; if the user provided path is a directory, then add at the end the initial name of the directory
			(concat
			 (file-name-as-directory raw-user-given-path)
			 file-name)
		      ;; else if the user provided a file name at the end of the path, use it
		      raw-user-given-path)))
		 (t
		  (concat
		   (file-name-as-directory linkin-org-store-directory)
		   file-name))))
	       ;; get the new raw file name
	       (new-raw-file-name
		(file-name-nondirectory new-raw-file-path))
	       ;; give the file name an id
	       (new-file-name
		(linkin-org-give-id-to-file-name new-raw-file-name))
	       ;; get the new file path
	       (new-file-path
		(if (not linkin-org-store-file-directly-p)
		    ;; if the user gave a new path
		    (concat
		     (file-name-as-directory
		      (file-name-directory new-raw-file-path))
		     new-file-name)
		  ;; else, move the directory to the store directory, except if it's already in there
		  (if is-file-already-in-store-directory?
		      (concat
		       ;; this just concats a "/" at the end of the directory (if there's none already)
		       (file-name-as-directory
			(file-name-directory
			 (expand-file-name
			  (directory-file-name file-path))))
		       new-file-name)
		    ;; else, just put the new path in the store directory
		    (concat
		     (file-name-as-directory
		      (expand-file-name
		       (directory-file-name linkin-org-store-directory)))
		     new-file-name)))))
	    ;; (message "new-raw-file-path: %s" new-raw-file-path)
	    ;; (message "new-raw-file-name: %s" new-raw-file-name)
	    ;; (message "new-file-path: %s" new-file-path)
	    ;; (copy-directory file-path new-file-path)
	    (rename-file file-path new-file-path)
	    ;; copy a link towards the stored directory
	    (linkin-org-perform-function-as-if-in-dired-buffer new-file-path 'linkin-org-dired-get-link)
	    ;; update the Dired buffer
	    (revert-buffer)))
      ;; if it's a file
      (progn
	(let* ((new-raw-file-path
		(cond
		 ((not linkin-org-store-file-directly-p)
		  (let ((raw-user-given-path
			 (read-directory-name
			  "Give new location: "
			  (file-name-as-directory linkin-org-store-directory)
			  (file-name-as-directory linkin-org-store-directory)
			  nil)))
		    (if (file-directory-p raw-user-given-path)
			;; if the user provided path is a directory, then add at the end the initial name of the directory
			(concat
			 (file-name-as-directory raw-user-given-path)
			 file-name)
		      ;; else if the user provided a file name at the end of the path, use it
		      raw-user-given-path)))
		 (t
		  (concat
		   (file-name-as-directory linkin-org-store-directory)
		   file-name))))
	       ;; get the new raw file name
	       (new-raw-file-name
		(file-name-nondirectory new-raw-file-path))
	       ;; give the file name an id
	       (new-file-name
		(linkin-org-give-id-to-file-name new-raw-file-name))
	       ;; get the new file path
	       (new-file-path
		(if (not linkin-org-store-file-directly-p)
		    ;; if the user gave a new path
		    (concat
		     (file-name-as-directory
		      (file-name-directory new-raw-file-path))
		     new-file-name)
		  ;; else, move the file to the store directory, except if it's already in there
		  (if is-file-already-in-store-directory?
		      (concat
		       ;; this just concats a "/" at the end of the directory (if there's none already)
		       (file-name-as-directory
			(file-name-directory
			 (expand-file-name
			  (directory-file-name file-path))))
		       new-file-name)
		    ;; else, just put the new path in the store directory
		    (concat
		     (file-name-as-directory
		      (expand-file-name
		       (directory-file-name linkin-org-store-directory)))
		     new-file-name)))))
	  (rename-file file-path new-file-path)
	  (linkin-org-perform-function-as-if-in-dired-buffer new-file-path 'linkin-org-dired-get-link)
	  ;; update the Dired buffer
	  (revert-buffer))))))

(defun linkin-org-open-link-and-do-function (string-link function-to-perform)
  "Open STRING-LINK, apply FUNCTION-TO-PERFORM, come back."
  (let (;; remember the current buffer and the current position of point
	(init-buffer (current-buffer))
	(init-point (point))
	;; save the current buffer list
	(init-buffer-list (buffer-list))
	new-buffer)
    (unwind-protect
	(progn
	  ;; Open to the link
	  (org-link-open string-link)
	  ;; call the function
	  (funcall function-to-perform)
	  ;; remember the buffer we landed in by opening the link
	  (setq new-buffer (current-buffer))
	  ;; save the buffer
	  (save-buffer))
      ;; kill the opened buffer if it was not open before
      (when (memq new-buffer
		  (cl-set-difference (buffer-list) init-buffer-list))
	(kill-buffer new-buffer))
      ;; save
      ;; ;; kill all buffers that were open
      ;; (mapcar
      ;;  (lambda (e) (with-current-buffer e
      ;;                (save-buffer) (kill-buffer)
      ;;                )
      ;;    )
      ;; (cl-set-difference (buffer-list) init-buffer-list)
      ;; )
      ;; go back where we were
      (switch-to-buffer init-buffer)
      (goto-char init-point))))




;;;; ------------------------------------------- file link type

;; To create a link towards the file under point in a Dired buffer
;;;###autoload
(defun linkin-org-dired-get-link ()
  "Return a link towards the file under point in Dired."
  (let* ((file-path (expand-file-name (dired-file-name-at-point)))
	 ;; remove the trailing slash if it's a directory
	 (file-path (directory-file-name file-path))
	 ;; the name of the file, without path
	 (file-name
	  ;; if the file under point is a directory
	  (if (file-directory-p file-path)
	      ;; remove the trailing slash, get the name of the directory
	      (file-name-nondirectory (directory-file-name file-path))
	    ;; else if it's a file
	    (file-name-nondirectory file-path)))
	 ;; file name without id, if there is an id in the file name
	 (file-name (linkin-org-strip-off-id-from-file-name file-name))
	 (file-name-sans-ext (file-name-sans-extension file-name))
	 (extension (file-name-extension file-name))
	 ;; tronque le nom du fichier s'il est trop long
	 (file-name
	  (if (> (length file-name) 70)
	      (concat
	       (substring file-name-sans-ext 0 50)
	       " [___] " "." extension)
	    file-name)))
    (if file-name
	;; if it's a file, not a directory
	(kill-new
	 (format "[[file:%s][[file] %s]]"
		 (linkin-org-escape-square-brackets file-path)
		 file-name)))
    ;; otherwise, remove the trailing slash
    (let* ((directory-name-without-slash
	    (directory-file-name file-path)))
      (format "[[file:%s][[file] %s]]"
	      (linkin-org-escape-square-brackets directory-name-without-slash)
	      file-name))))






;; to get a link towards the current line in an editale file.
;; if there is an id in the current line, use it. Otherwise use the line number.
;;;###autoload
(defun linkin-org-get-inline ()
  "Return a link towards the current line in an editable file.
If there is an inline id in the current line, use it.
Otherwise use the line number."
  (let* ((current-file-path
	  (when (buffer-file-name)
	    (expand-file-name (buffer-file-name))))
	 (file-name
	  (when current-file-path
	    (file-name-nondirectory current-file-path)))
	 ;; get the current line in string
	 (current-line
	  (buffer-substring-no-properties
	   (line-beginning-position)
	   (line-end-position)))
	 ;; get the id in the current line, if there is one
	 (inline-id-with-prefix
	  (linkin-org-extract-id current-line linkin-org-inline-id-regexp))
	 ;; remove the leading id: part of the inline id
	 (inline-id
	  (when inline-id-with-prefix
	    (replace-regexp-in-string "id:" "" inline-id-with-prefix)))
	 ;; get the text that is after the id part, if any
	 ;; if the
	 (raw-text-after-id
	  (when inline-id
	    (let* ((text
		    (cadr
		     (split-string current-line inline-id-with-prefix)))
		   ;; remove the leading ] if there is one
		   (text
		    (if (string-prefix-p "]" text)
			(substring text 1)
		      text)))
	      ;; if the text is only made of spaces, return nil
	      (if (string-match-p "\\`[[:space:]]*\\'" text)
		  nil
		;; else, delete the leading and trailing spaces
		(replace-regexp-in-string
		 (rx
		  (or
		   (seq line-start (zero-or-more space))
		   (seq (zero-or-more space) line-end)))
		 ""
		 text)))))
	 ;; shorten the text after id if it's too long, say larger than 70 characters
	 (shortened-text-after-id
	  (when raw-text-after-id
	    (if (> (length raw-text-after-id) 70)
		(concat (substring raw-text-after-id 0 50) " [___]")
	      raw-text-after-id)))
	 (line-number (line-number-at-pos)))
    ;; get an id only if the file has a path
    (if file-name
	(if inline-id
	    (if shortened-text-after-id
		(format
		 "[[file:%s::(:inline-id %s)][[file] \"%s\"]]"
		 current-file-path
		 inline-id
		 ;; (linkin-org-strip-off-id-from-file-name file-name)
		 shortened-text-after-id)
	      (format
	       "[[file:%s::(:inline-id %s)][[file] %s]]"
	       current-file-path
	       inline-id
	       (linkin-org-strip-off-id-from-file-name file-name)))
	  (format "[[file:%s::%d][[file] %s _ l%d]]"
		  current-file-path
		  line-number
		  (linkin-org-strip-off-id-from-file-name file-name)
		  line-number))
      ;; else if the file has no path, do nothing
      (message "linkin-org: this buffer has no path, cannot produce a link towards it.")
      nil)))


;; to leave an id in an editable line
;;;###autoload
(defun linkin-org-store-inline ()
  "Leave an id in an editable line and copy a link towards it."
  (let ((id (linkin-org-create-id))
	(range
	 (list
	  (line-beginning-position)
	  (goto-char (line-end-position 1))))
	(current-line
	 (buffer-substring-no-properties
	  (line-beginning-position)
	  (line-end-position))))
    ;; insert an inline id only if there is none already
    (when (not (linkin-org-extract-id current-line))
      (save-excursion
	;; if the current line is not commented, comment it.
	(if (eq (apply #'min range) (apply #'max range))
	    ;; if the current line is empty, do special treatment since I cannot easily comment an empty line
	    (progn
	      ;; insert the id
	      (insert "[id:" id "] ")
	      ;; comment the line
	      (comment-or-uncomment-region
	       (line-beginning-position)
	       (goto-char (line-end-position 1))))
	  ;; else, if the line is non empty
	  ;; do a differenet treatment if the mode is org-mode, since there's a bug there
	  ;; (if (eq major-mode 'org-mode)
	  (if (derived-mode-p 'org-mode)
	      (progn
		;; check if the first characters are # followed by a space
		(unless (string-match-p "^# " current-line)
		  ;; if not, then comment the line
		  ;; go to the beginning of the line and insert a hashtag
		  (beginning-of-line)
		  (insert "# "))
		;; return to the beginning of the line
		(beginning-of-line)
		;; go two char forward to skip the # and the space
		(forward-char 2)
		;; insert the id
		(insert "[id:" id "] "))
	    ;; else if the mode is not org-mode, just comment the line
	    (let ;; check if there are comments, if yes go at the beginning of them
		((comments-p (comment-beginning)))
	      (unless comments-p
		;; if there are no comments to go at the beginning to, comment the whole line and go at the beg of comments
		(comment-or-uncomment-region
		 (apply #'min range)
		 (apply #'max range))
		(comment-beginning))
	      ;; just insert the id at the beginning of the comments
	      (insert "[id:" id "] "))))))
    ;; copy the link
    (linkin-org-get)
    ;; go the end of the line
    (end-of-line)))





;;;; ------------------------------------------- main commands
(defun linkin-org-leave-id-to-file-in-dired ()
  "Add an id to the under point in Dired.
Do nothing if the file already has an id."
  (interactive)
  (let* ((file-path (dired-file-name-at-point))
	 ;; t if file-path is a directory, nil if it's a file
	 (is-directory? (file-directory-p file-path))
	 ;; get the file-name, or directory name
	 (file-name
	  (if is-directory?
	      (file-name-nondirectory (directory-file-name file-path))
	    (file-name-nondirectory file-path)))
	 ;; get the file path without the name of the file
	 (file-path-sans-name
	  (if is-directory?
	      (file-name-directory (directory-file-name file-path))
	    (file-name-directory file-path))))
    ;; if the file doesnt already has an id, rename the file or directory with an id at the front
    (unless (linkin-org-extract-id file-name)
      (rename-file
       file-path
       (concat
	file-path-sans-name
	(linkin-org-give-id-to-file-name file-name)))
      (revert-buffer))))


(defun linkin-org-get ()
  "Kill a link towards what is under point."
  (interactive)
  (cond
   ;; if in a Dired buffer, get a link towards the file under point
   ((string= (symbol-name major-mode) "dired-mode")
    (kill-new (linkin-org-dired-get-link)))
   ;; else, get a link towards the current line of the buffer
   ((not buffer-read-only)
    (let ((inline-link (linkin-org-get-inline)))
      ;; copy the link only if a non-nil link was computed
      (when inline-link (kill-new (linkin-org-get-inline)))))))





;;;###autoload
(defun linkin-org-store ()
  "Store what is under point and kill a link to it."
  (interactive)
  (let* ((mode (symbol-name major-mode)))
    (cond
     ;; If in a Dired buffer
     ((string= mode "dired-mode")
      (linkin-org-store-file))
     ;; If in an editable buffer
     ((not buffer-read-only)
      (linkin-org-store-inline)))))






;;;###autoload
(defun linkin-org-file-open (link)
  "Open the file at LINK."
  (let* ((file-path (org-element-property :path link))
	 ;; (metadata (org-element-property :metadata link))
	 (line-number-or-id (org-element-property :search-option link)))
    (when (file-exists-p file-path)
      ;; open the file from a Dired buffer using the function `linkin-org-open-file-as-in-dired'
      (linkin-org-perform-function-as-if-in-dired-buffer
       file-path
       linkin-org-opening-file-function-in-dired)
      ;; go to the id if specified
      (when line-number-or-id
	(cond
	 (;; if line-number-or-id matches an id, search for that id in the buffer
	  (linkin-org-extract-id line-number-or-id)
	  (org-link-search line-number-or-id))
	 (;; else, if it matches a number, go to that line number
	  (string-match-p "^[0-9]+$" line-number-or-id)
	  (org-goto-line (string-to-number line-number-or-id))))))))


;;;###autoload
(defun linkin-org-resolve-link (link &optional no-path-resolving)
  "Parse the metadata in LINK.
Returns a link with a resolved path and an additional :metadata property.
Takes as input and return a link in org element form.
If NO-PATH-RESOLVING is non-nil, do not resolve the path of the link."
  (let* (;; get the raw link, that is, the string containing the data of the link
	 (link-raw-link (org-element-property :raw-link link))
	 ;; get the type of the link
	 (link-type (org-element-property :type link))
	 ;; (link-raw-path (org-element-property :path link))
	 ;; get data of the link, that is, the interior of the link minus the type (ie, file:)
	 (link-path (org-element-property :path link))
	 ;; extract the path, that is, the substring of link-path before the first :: if there is one
	 (link-path (car (string-split link-path "::")))
	 ;; get the metadata, that is, the alist after the last "::" in the link-raw-link
	 (link-metadata
	  (let ((index (string-match "::" link-raw-link))
		(link-parts (split-string link-raw-link "::")))
	    ;; if there is metadata
	    (when index
	      ;; get the string after the last "::"
	      (read (car (last link-parts))))))
	 (link-inline-id
	  (when (plist-get link-metadata :inline-id)
	    (symbol-name (plist-get link-metadata :inline-id)))))
    ;; compute the new path, if we should resolve the path for that link type
    (when (and
	   link-path
	   (member link-type linkin-org-link-types-to-check-for-id)
	   (not no-path-resolving))
      (org-element-put-property link :path
				(linkin-org-resolve-path link-path)))
    (when link-metadata
      (org-element-put-property link :metadata link-metadata))
    ;; if the link has an inline id, add it to the link as a search string value
    (if link-inline-id
	(org-element-put-property link :search-option
				  (concat "id:" link-inline-id))
      ;; else, check if the original search option is an id
      (if-let (id
	       (linkin-org-extract-id
		(org-element-property :search-option link)))
	  ;; then add the id: keyword to the search option
	  (org-element-put-property link :search-option
				    (concat "id:" id))))
    link))

;;;###autoload
(defun linkin-org-redirect-link-opening (std-link-opening-function link &optional _args)
  "Open the LINK.
If `linkin-org-open-links-as-in-dired-p' is non-nil, open it as if in Dired.
Otherwise, calls the function STD-LINK-OPENING-FUNCTION to open it."
  (let (;; resolve the link, so that it has a correct path and metadata
	(link (linkin-org-resolve-link link)))
    (if (and linkin-org-open-links-as-in-dired-p
	     (string= (org-element-property :type link) "file"))
	;; open the link as in Dired if it's a file link and if the user wants to
	(linkin-org-file-open link)
      ;; else call the standard link opening function
      (funcall std-link-opening-function link link))))

;;;###autoload
(define-minor-mode linkin-org-mode
  "Minor mode to open your links with linkin-org."
  :init-value nil
  :lighter nil
  (if linkin-org-mode
      ;; replace org-lin-open with linkin-org-open, so that Dired is used to open links in case it is asked
      (advice-add 'org-link-open :around #'linkin-org-redirect-link-opening)
    ;; (advice-add 'org-link-open :filter-args #'plug-in-org-link-open)
    ;; (advice-remove 'org-link-open #'linkin-org-link-open)
    (advice-remove 'org-link-open #'linkin-org-redirect-link-opening)))

;;;###autoload
(defun linkin-org-turn-on-minor-mode ()
  "Turn on linkin-org minor mode."
  (interactive)
  (linkin-org-mode 1))

;;;###autoload
(define-global-minor-mode linkin-org-global-mode linkin-org-mode linkin-org-turn-on-minor-mode)


(provide 'linkin-org)

;;; linkin-org.el ends here

