;;; linkin-org.el --- Use fast and robust links in org mode

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


(require 'pdf-tools)

;;;; paterns
;; this is the pattern for the id of a file
(defconst linkin-org-id-pattern
  (rx
   (or
    (seq (= 4 digit) (= 2 digit) (= 2 digit) "T" (= 2 digit) (= 2 digit) (= 2 digit))
    (seq (= 4 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit) (= 2 digit))
    (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "--" (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) (or (seq) (seq "--" (= 5 digit))))
    )
   )
  )

;; separator between the id and the original file name
(defconst linkin-org-sep-pattern
  (rx (or "--" "-"))
  )

(defconst mon-dossier-fourre-tout
  "~/Dropbox/FourreTout/"
  )


;;;; helper function
(defun split-string-at-double-colon (input)
  "Split INPUT string into two parts at the first occurrence of '::'.
The second part starts with '::'. If '::' is not in the string, return
the original string as the first part and nil as the second part."
  (let
      (
       (index (string-match "::" input))
       )
    (if index
        (list (substring input 0 index) (substring input index))
      (list input nil))))


(defun linkin-org-get-file-name-id (file-name)
  "tell if a file name has an id, returns the id string if yes, nil otherwise"
  (interactive)
  (if (string-match linkin-org-id-pattern file-name)
      ;; this function returns a list of list of strings
      (car (car (s-match-strings-all linkin-org-id-pattern file-name)))
    nil
    )
  )

(defun linkin-org-remove-id-from-file-name (file-name)
  "take a file name and strip off the id part"
  (interactive)
  (if (string-match linkin-org-id-pattern file-name)
      ;; this function returns a list of list of strings
      (let*
	  (
	   ;; remove the heading id
	   (file-name-without-id (replace-regexp-in-string linkin-org-id-pattern "" file-name))
	   ;; remove the heading sep -- if there is one
	   (file-name-without-sep (replace-regexp-in-string (concat "^" linkin-org-sep-pattern) "" file-name-without-id))
	   )
	file-name-without-sep
	)
    file-name
    )
  )

(defun linkin-org-transform-square-brackets (str)
  "Unescape occurrences of '\\\\', '\\[', and '\\]' in INPUT string."
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

(defun parse-org-link (link-string)
  "Parse LINK-STRING into an Org element and return the result."
  (with-temp-buffer
   (let ((org-inhibit-startup nil))
     (insert link-string)
     (org-mode)
     (goto-char (point-min))
     (org-element-link-parser))
   )
  )


(defun find-first-matching-file (prefix dir)
  "Use ripgrep (rg) to find the first file matching PREFIX in DIR recursively.
Returns the file path as a string or nil if not found."
  (let* (
	 (dir (expand-file-name dir))
	 (rg-command (format "(rg -g \"%s*\" --files %s & find \"%s\" -type d -name \"%s*\") | head -n 1" prefix dir dir prefix))
	 file-path
	 )
    (with-temp-buffer
      (call-process-shell-command rg-command nil (current-buffer) nil)
      (unless (zerop (buffer-size))
        (setq file-path (car (string-lines (buffer-string))))
	)
      )
    file-path
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


;; just take a list of two strings and make a link
(defun take-list-of-two-strings-and-make-link (l &optional description)
  (if description
      (format "[[%s][%s %s]]" (car l) description (cadr l))
    (format "[[%s][%s]]" (car l) (cadr l))
    )
  )


;;;; find the linked file

(defun linkin-org-is-link-path-correct (file-path)
  "returns the path to the file if it exists, use id utlimately to find the file"
  ;; if the file path exists, just return it
  (cond
   (
    (file-exists-p file-path)
    file-path
    )
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
	  ;; ;; list all files in the file directory
	  ;; (dolist
	  ;;     ;; third t in directory-files-recursively is to include directories
	  ;;     (tmp-file (directory-files-recursively file-dir ".*" t t) result)
	  ;;   ;; Ensure the file or directory exists
	  ;;   (let
	  ;; 	(
	  ;; 	 (tmp-file-name
	  ;; 	  (if (equal (file-name-directory tmp-file) tmp-file)
	  ;; 	      ;; if the file path is a directory
	  ;; 	      (file-name-nondirectory (directory-file-name tmp-file))
	  ;; 	    ;; else if the file path is a file
	  ;; 	    (file-name-nondirectory tmp-file)
	  ;; 	    )
	  ;; 	  )
	  ;; 	 )
	  ;;    (when (and (file-exists-p tmp-file)
	  ;; 		(string-prefix-p id-of-file-name tmp-file-name)
	  ;; 		)
          ;;      (setq result tmp-file)
	  ;;      )
	  ;;    )
	  ;;   )
	)
      )
    )
   ;; if the id search failed, just return the file path
   (t
    file-path
    )
   )
  )

(defun linkin-org-turn-link-into-correct-one (link-string)
  "take a link as a string and returns the same link but with a correct path"

  ;; get the link's type
  (let*
      (
       ;;turn the string link into an org element
       (link (parse-org-link link-string))
       ;; get the path of the link
       (link-type (org-element-property :type link))
       )
    
    ;; if it's one of my home made links
    (if (and link-type
	     (member link-type '("mpd" "pdf" "video"))
	     )
        (let*
	    (
	     ;; get the path of the link
	     (link-raw-path (org-element-property :path link))
	     ;; strip of the metadata from the path
	     (link-path (car (split-string-at-double-colon link-raw-path)))
	     (link-metadata (car (cdr (split-string-at-double-colon link-raw-path))))
	     ;; if the link has a path, then change it to the correct path
	     (new-link-path (if link-path
				(linkin-org-is-link-path-correct link-path))
			    )
	     ;; rebuild the link's correct path and metadata
	     (new-link-path (linkin-org-link-escape (concat new-link-path link-metadata)))
	     ;; build a new link based on that path
	     (new-link-string (concat "[[" link-type ":" new-link-path "]]"))
	     )
	  new-link-string
	  )
      ;; if it's a normal link
      (let*
	  (
	   ;; get the path of the link
	   (link-raw-path (org-element-property :path link))
	   ;; if the link has a path, then change it to the correct path
	   (new-link-path (if link-raw-path
			      (linkin-org-is-link-path-correct link-raw-path))
			  )
	   ;; build a new link based on that path
	   (new-link
	    (if new-link-path
		(org-element-put-property link :path new-link-path)
	      link-string
	      )
	    )
	   ;; turn this new link into string
	   (new-link-string (org-element-interpret-data new-link))
	   )
	new-link-string
	)
      )
    )
  )

(defun linkin-org-get-org-link-string-under-point ()
  "returns the string of an org link under point, returns nil if no link was found"
  (interactive)
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

(defun linkin-org-open-link-at-point ()
  (interactive)
  (if-let (
	   ;; get the link under point in string form
	   (link-string (linkin-org-get-org-link-string-under-point))
	   ;; change the string link into a correct link (with correct path, following id)
	   (new-link-string (linkin-org-turn-link-into-correct-one link-string))
	   )
      (progn
	(with-temp-buffer
	;; (with-current-buffer (create-file-buffer "/home/juliend/test")
	  (let ((org-inhibit-startup nil))
	    ;; (message (concat "mon LINK: " new-link-string))
	    (insert new-link-string)
	    (org-mode)
	    (goto-char (point-min))
	    (org-open-at-point)
	    )
	  )
	t
	)
    )
  )

;; tries to open the link under point, otherwise fallback to org-open-at-point-global
(defun linkin-org-open-at-point ()
  (interactive)
  (linkin-org-open-link-at-point)
  ;; (unless (linkin-org-open-link-at-point)
  ;;   (org-open-at-point-global)
  ;;   )
  )

;;;; file link

;; pour copier un lien vers le fichier texte courant
(defun linkin-org-lien-fichier-et-ligne-du-curseur ()
  (interactive)
  (let*
      (
       (chemin-fichier-actuel (abbreviate-file-name (buffer-file-name)))
       (nom-fichier-actuel-sans-chemin (file-name-nondirectory chemin-fichier-actuel))
       (ligne-actuelle (line-number-at-pos))
       (lien (format "[[file:%s::%d][[file] %s _ l%d]]"
		     chemin-fichier-actuel
		     ligne-actuelle
		     nom-fichier-actuel-sans-chemin
		     ligne-actuelle
		     )
	     )
       )
    lien
    )
  )

;; Pour créer un lien org vers le fichier sous le curseur
;; place le lien dans le kill ring
(defun linkin-org-dired-get-link ()
  (interactive)
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
	 (nom-fichier (linkin-org-remove-id-from-file-name nom-fichier))
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
      (kill-new (format "[[file:%s][[file] %s]]" directory-name-without-slash nom-fichier))
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

(defun linkin-org-file-open (link)
  "Open the file at LINK."
  (let* (
	 (link-parts (split-string link "::"))
	 (file-path (car link-parts))
	 (line-number-or-id (cadr link-parts))
	 (column-number (caddr link-parts))
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
	      (if (string-match linkin-org-id-pattern line-number-or-id)
		 (progn
		   (save-excursion (progn
				     (beginning-of-buffer)
				     (setq id-position (if (re-search-forward line-number-or-id nil t 1) (point) nil))
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
      (message "Neither the file nor the id could be found")
      )
    )
  )

;;;; music link
(org-add-link-type "mpd" 'org-mpd-open nil)
;; ishould use this instead
;; (org-link-set-parameters TYPE &rest PARAMETERS)


(defun org-mpd-link-get-path (link)
  (let* (
         (link-parts (split-string link "::"))
         ;; for the mpd file path
         (mpd-file (car link-parts))
         )
    mpd-file
    )
  )
  
(defun org-mpd-open (link)
  """
  link is a string containing
the paths to the song (an mp3 file or so, or a .cue file with a trailing /track<number>) as a lisp list, each song is a string element of the list
then "::",
then, a timestamp in format readable by mpd, for instance 1:23:45
  """

  (let* (
	 ;; unescape the link
	 ;; (link (unescape-special-characters link))
	 (link-parts (split-string link "::"))
	 ;; use the read function that parses a string as code
	 (songs (read (car link-parts)))
	 (timestamp (cadr link-parts))
	 )
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


;; build the link
(defun linkin-org-lien-mpd-mingus ()
  (interactive)
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
	(format "[[mpd:(\"%s\")::00:00:00][[music] %s]]" (linkin-org-transform-square-brackets track-path) title)
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

;;;; pdf link
(org-add-link-type "pdf" 'org-pdf-open nil)
;; (setq linkin-org-open-pdf-link-other-frame nil)
;; (org-link-set-parameters "pdf" (:follow 'org-pdf-open))

(defun org-pdf-open (link)
  "Where page number is 105 and the positions to hightlight on the page are e1, e2, e3, e4, then the link should look like:
   [[pdf:/path/to/file.pdf::105::e1;e2;e3;e4][My description.]]"
  (let* (
	 (path+page+edges (split-string link "::"))
	 ;; for the pdf file path
         (pdf-file (car path+page+edges))

	 ;; for the page
         (page (string-to-number (car (cdr path+page+edges))))

	 ;; for the edges
	 (edges-str (car (cdr (cdr path+page+edges))))
	 ;; separate the edges by |
	 (edges-list-str (unless (not edges-str) (split-string edges-str "[;|]")))
	 ;; convert from string to int, get a list of four floating points numbers, that's the edges
	 (edges-list (unless (not edges-list-str) (list (mapcar 'string-to-number edges-list-str))))
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

	       (not linkin-org-open-org-link-other-frame)
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
	 (file-name-sans-id (linkin-org-remove-id-from-file-name file-name))
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
	 ;; truncate the selected text
	 (selected-text-tronque (if (and selected-text (> (length selected-text) 15))
				     (concat (substring selected-text 0 15) "...")
				   selected-text
				   )
				 )
	 ;; for the edges
	 (edges (if (pdf-view-active-region-p)
		    (pdf-view-active-region)
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
	      ;; edges are actually outputed as a list of Lists-trees
	      (edges (car edges))
	      ;; concat the edges with |
	      (string-edges (mapconcat #'prin1-to-string edges ";"))
	      )
	   (format "[[pdf:%s::%s::%s][[pdf] %s _ p%s _ \"%s\"]]" file page string-edges nom-fichier-tronque page selected-text-tronque)
	   )
	 )
      ;; else, no selected text, just care about the path and page
      (format "[[pdf:%s::%s][[pdf] %s _ p%s]]" file page nom-fichier-tronque page)
      )
    )
  )


;;;; video link
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
       ;; if it's a local file
       ((linkin-org-is-link-path-correct video-address)
	(progn
	  (message (format  "Playing %s at %s" video-address timestamp))
	  (start-process "mpv" nil "mpv" (format "--start=%s" timestamp) video-address)))
       ;; if it's no file path, check if it's a url
       ((linkin-org-is-url video-address)
	(progn
	  ;; print timestamp and video address
	  ;; (message "hello")
	  ;; (message (concat "video address : " ))
	  ;; (message timestamp)
	  ;; (message (format  "Playing from YOUTUBE %s at %s" video-address timestamp))
	  (start-process "mpv" nil "mpv" "--ytdl=no" (format "--start=%s" timestamp) video-address)
	  )
	)
       (t
	(message "Not a valid video file or url")
	)
       
       )
      )
    )
  )

;; Fonction polymorphe (dépendante du mode majeur) pour copier la chose sous le curseur dans le presse-papier


;;;; general purpose functions


(defun copie-dans-presse-papier ()
  (interactive)
  (let*
      ((mode (symbol-name major-mode)))
    (cond
     ;; Si on est dans un pdf, copie un lien vers le pdf
     ((string= mode "pdf-view-mode")
      (kill-new (linkin-org-pdf-get-link))
      )
     ;; Si du texte est sélectionné, copie simplement la sélection
     ((region-active-p)
      (kill-ring-save (region-beginning) (region-end))
      )
     ;; Si on est dans un buffer Dired, copie le fichier
     ((string= mode "dired-mode")
      (linkin-org-dired-get-link)
      )
     ;; Si on est dans un mail
     ((or (string= mode "mu4e-view-mode") (string= mode "mu4e-headers-mode"))
      (kill-new (take-list-of-two-strings-and-make-link (call-interactively 'org-store-link) "[mail]"))
      )
     ;; Si on est dans mingus playlist
     ((string= mode "mingus-playlist-mode")
      (kill-new (linkin-org-lien-mpd-mingus))
      )
     ;; Si on est simple mpc
     ((string= mode "simple-mpc-mode")
      (kill-new (linkin-org-link-mpd-simple-mpc))
      )


     ;; Sinon, place un lien vers la ligne du fichier courant dans le registre
     (t
      (kill-new (linkin-org-lien-fichier-et-ligne-du-curseur))
      )

     )
    )
  )


;;;; open link
(setq linkin-org-open-org-link-other-frame t)
(setq linkin-org-open-org-link-in-dired t)

(defun linkin-org-open-link-in-dired-other-frame ()
  (interactive)
  (if (region-active-p)
      ;; if a region is selected, then open all links in the region, in order
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
	    ;; 	(linkin-org-open-at-point)
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
		(linkin-org-open-at-point)
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
    ;; else open the link in the normal way
    (linkin-org-open-at-point)
    )

  )



;; to do an action on a file as if the point was on that file in dired
(defun linkin-org-perform-function-as-if-in-dired-buffer (file-path function-to-perform)
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
   

;; to open a file as if 
(defun linkin-org-open-file-as-in-dired (file-path)
  ;; (linkin-org-perform-function-as-if-in-dired-buffer file-path 'dired-open-file)
  (find-file file-path)
  )

;; code to yank the link of a given file
(defun linkin-org-yank-link-of-file (file-path)
  (linkin-org-perform-function-as-if-in-dired-buffer file-path 'linkin-org-dired-get-link)
  )

(defun linkin-org-open-link-directly-other-frame ()
  (interactive)
  (setq linkin-org-open-org-link-in-dired nil)
  (setq linkin-org-open-org-link-other-frame nil)
  (linkin-org-open-at-point)
  (setq linkin-org-open-org-link-other-frame t)
  (setq linkin-org-open-org-link-in-dired t)
  )


;;;; create link

(defun linkin-org-create-id ()
  "Return a string with the current year, month, day, hour, minute, second, and milliseconds."
  (let*
      (
       ;; (five-random-numbers (cl-loop for i from 1 to 5 collect (random 10)))
       (time-string (format-time-string "%Y%m%dT%H%M%S" (current-time)))
       )
    (concat time-string
	    ;; "--"
	    ;; (mapconcat 'number-to-string five-random-numbers )
	    ;; "--"
	    )
    )
  )


;; Fonction pour renommer et copier le fichier ou dossier sous le curseur dans le Fourre-tout
;; 
(defun linkin-org-store (&optional yank-link?)
  (interactive)
  (let* (
	 (chemin-fichier-ou-dossier (dired-file-name-at-point))
	 (is-file-already-in-fourre-tout? (s-prefix?
					   (expand-file-name mon-dossier-fourre-tout)
					   (expand-file-name chemin-fichier-ou-dossier)
					   )
					  )
	 )
    ;; check wether it's a file or a directory
    (if (file-directory-p chemin-fichier-ou-dossier)
	;; if it's a directory
	(progn
	  (let
	      (
	       ;; ask for the directory new name
	       ;; ~directory-file-name~ removes the trailing slash so that ~file-name-nondirectory~ returns the last part of the path
	       (nouveau-nom (read-string "Nouveau nom : " (file-name-nondirectory (directory-file-name chemin-fichier-ou-dossier))))
	       (id (concat (linkin-org-create-id) linkin-org-sep-pattern))
	       )
	    (if is-file-already-in-fourre-tout?
		(rename-file chemin-fichier-ou-dossier (concat mon-dossier-fourre-tout id nouveau-nom))
	      (copy-directory chemin-fichier-ou-dossier (concat mon-dossier-fourre-tout id nouveau-nom))
	      )
	    )
	  )
      ;; if it's a file
      (progn
	(let*
	    ;; ask for the file new name
	    (
	     (nouveau-nom (read-string "Nouveau nom : " (file-name-nondirectory chemin-fichier-ou-dossier)))
	     (id (concat (linkin-org-create-id) linkin-org-sep-pattern))
	     (nouveau-nom (concat id nouveau-nom))
	     (complete-file-path (concat mon-dossier-fourre-tout "/" nouveau-nom))
	     )
	  (if is-file-already-in-fourre-tout?
	      (rename-file chemin-fichier-ou-dossier complete-file-path)
	    (copy-file chemin-fichier-ou-dossier complete-file-path)
	    )
	  (linkin-org-yank-link-of-file complete-file-path)
	  )
	
	)
      )
    )
  )

;; To leave an id in a writeable text file

;; classic comment-line function with a few convenience changes
(defun linkin-org-comment-line ()
  (interactive "p")
  (let ((range
         (list (line-beginning-position)
               (goto-char (line-end-position 1))
	       )))
    (comment-or-uncomment-region
     (apply #'min range)
     (apply #'max range)))
  (end-of-line)
  )

;; to leave an id in an editable line
(defun linkin-org-leave-text-id (&optional yank-link?)
    (interactive)
    (let (
	  (id (linkin-org-create-id))
	  (range
           (list (line-beginning-position)
		 (goto-char (line-end-position 1))
		 )
	   )
	  )
    ;;   ;; make sure the line is not commented
    ;; (when (comment-only-p (apply #'min range) (apply #'max range))
    ;;   (comment-or-uncomment-region
    ;;    (apply #'min range)
    ;;    (apply #'max range))
    ;;   )
    ;; ;; go to the beginning of the line and insert an id
    ;; (back-to-indentation)
    ;; (insert (concat "[" id "] "))

    ;; ;; [20250311T224321] comment the line
    ;; (let
    ;; 	(
    ;; 	 (range (list (line-beginning-position)
    ;; 		      (goto-char (line-end-position 1))
    ;; 		      )
    ;; 		)
    ;; 	 )
    ;;   (comment-or-uncomment-region
    ;;    (apply #'min range)
    ;;    (apply #'max range)
    ;;    )
    ;;   )

      ;; quick fix for org-mode
      (if (and
	   (not (eq major-mode 'org-mode))
	   (comment-only-p (apply #'min range) (apply #'max range))
	   )
	(progn
	  ;; go to the beginning of the commented text
	  (comment-beginning)
	  (insert (concat "[" id "] "))
	  )
      ;; else, insert at the beginning of line and comment the line
      (progn
       (back-to-indentation)
       (insert (concat "[" id "] "))
       (comment-region (line-beginning-position) (line-end-position))
       ;; (comment-or-uncomment-region
       ;; 	(apply #'min range)
       ;; 	(apply #'max range)
       ;; 	)
       )
      )
    ;; go to the end of line
    (end-of-line)

    ;; if we want to yank the link
    (let*
	(
	 (file-path (buffer-file-name))
	 (file-name (when file-path
		     (file-name-nondirectory file-path)
		     )
		    )
	 )
      ;; kill a link only if the current buffer has a valid file path
      (if file-path
       (kill-new (format
		  "[[file:%s::%s][[file] %s]]"
		  file-path
		  id
		  (linkin-org-remove-id-from-file-name file-name)
		  )
		 )
       (message "Current buffer is not attached to a file, no link was created.")
       )
      )
    )
    )

(provide 'linkin-org)
