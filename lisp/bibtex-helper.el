(require 'bibtex)

(defvar bibtex-helper-entry-type-regexp "^@\\([a-zA-Z_]+\\){" "Matches the entry type of a BibTeX record.")
(defvar bibtex-helper-entry-key-regexp "^@[a-zA-Z_]+{\\([^,]+\\)," "Matches the key of a BibTeX record.")
(defvar bibtex-helper-file-translation-programs '(("pdf" "pdftotext %s -") ("doc" "antiword %s")) "Alist which matches an extension (as a string) to a command (as a string) to be used along with a filename in a call to `format' to construct a command to translate the file of the given extension to a text stream.")
(defvar bibtex-helper-translation-bugs '(("ﬀ" "ff") ("ﬁ" "fi") ("ﬃ" "ffi") ("ﬂ" "fl") ("͑" "(") ("m͒" ")")) "Alist of regular expressions and their replacements needed to clean some translation bugs")
(defvar bibtex-helper-file-view-programs '(("pdf" "evince") ("doc" "oowriter") ("dvi" "xdvi")) "Alist which matches an extension (as a string) to a command (as a string) to be used to construct a command to view the file of the given extension.")

(defvar bibtex-papers-directory "/var/lib/bibtex" "A directory into which to move the original source copy when filing.")

(defun bibtex-helper-buffer (&optional buffer-name)
  "Return the buffer in BUFFER-NAME or return the buffer visiting `bibtex-database' if nil."
  (if buffer-name
      (get-buffer buffer-name)
    (if (memq major-mode '(bibtex-mode))
	(current-buffer)
      (find-file-noselect bibtex-database))))

(defun bibtex-helper-field-regexp (field)
  (concat "[ 	]*" field "[ 	]*=[ 	]*{?.*}?,?"))

(defun bibtex-helper-append-file-from-key ()
  "Automatically add a file field with value determined by
`bibtex-papers-directory' and the key of the record at point."
  (bibtex-helper-set-field
   "file"
   (path bibtex-papers-directory (format "%s.pdf" (bibtex-helper-get-current-key)))))
(add-hook 'bibtex-clean-entry-hook 'bibtex-helper-append-file-from-key)

(defun bibtex-helper-all-keys-in-buffer (&optional buffer)
  "Return a list of the different keys in BUFFER, defaults
to the file in `bibtex-database'."
  (let ((keys (list)))
    (save-window-excursion
      (set-buffer (bibtex-helper-buffer))
      (beginning-of-buffer)
      (while (re-search-forward bibtex-helper-entry-key-regexp nil t)
	(when (not
	       (member
		(match-string 1)
		keys))
	  (setq keys
		(append keys (list (strip-properties (match-string 1))))))))
      keys))

(defun bibtex-helper-all-entry-types-in-buffer (&optional buffer)
  "Return a list of the different entry types in BUFFER, defaults
to the file in `bibtex-database'."
  (save-window-excursion
    (set-buffer (bibtex-helper-buffer))
    (beginning-of-buffer)
    (let ((entry-types (list)))
      (while (re-search-forward bibtex-helper-entry-type-regexp)
	(when (not
	       (member
		(match-string 1)
		entry-types))
	  (setq entry-types
		(append entry-types (match-string 1)))))
      (entry-types))))

(defun bibtex-helper-get-current-key (&optional buffer)
  "Return the key of the present record in the BibTeX BUFFER or
in a buffer visiting the `bibtex-database' if BUFFER is nil."
  (let ((key nil))
    (save-excursion
      (set-buffer (bibtex-helper-buffer))
      (bibtex-beginning-of-entry)
      (when (re-search-forward bibtex-helper-entry-key-regexp nil t)
	(setq key (match-string 1))
	(set-text-properties 0 (length key) nil key))
      key)))

(defun bibtex-helper-current-entry ()
  "Return the entry at point as a string."
  (save-excursion
    (beginning-of-line)
    (let ((starting-point
	   (progn
	     (while (and
		     (not (bobp))
		     (not (looking-at "^@")))
	       (previous-line)
	       (beginning-of-line))
	     (point))))
      (forward-line)
      (while (and
	      (not (eobp))
	      (not (looking-at "^@")))
	(forward-line)
	(beginning-of-line))
      (buffer-substring-no-properties starting-point (point)))))

(defun bibtex-helper-jump-to-key (key &optional show-file buffer)
  "Jump to the entry corresponding to KEY in BUFFER (defaults to
a buffer visiting `bibtex-database')."
  (interactive "P")
  (if (not key)
      (setq key (completing-read "Key: " bibtex-reference-keys nil t)))
  (switch-to-buffer (bibtex-helper-buffer buffer))
  (beginning-of-buffer)
  (while (not
	  (string= (bibtex-helper-get-current-key) key))
    (re-search-forward bibtex-helper-entry-type-regexp))
  (beginning-of-line)
  (when show-file
    (if (file-exists-p (bibtex-helper-get-field "file"))
	(bibtex-helper-view-file)
      nil)))

(defun bibtex-helper-file-paper (path key &optional update-bibtex buffer)
  "Rename PATH based upon KEY and file it in my papers directory.
If optional UPDATE-BIBTEX then update the bibtex BUFFER (defaults
to the corresponding entry in the `bibtex-database')."
  (interactive "fPath: \nMKey: \nP")
  (when (not (file-exists-p path)) (error "No such file %s" path))
  (let* ((extension (file-name-extension path))
	 (new-path (path bibtex-papers-directory (concat key "." extension))))
    (rename-file path new-path 1)
    (message (format "Renamed %s to %s" path new-path))
    (if update-bibtex
	(save-window-excursion
	  (bibtex-helper-jump-to-key key buffer)
	  (bibtex-helper-set-field "file" new-path)))))
    
(defun bibtex-helper-get-field (field &optional buffer)
  "Return the value of FIELD for the entry at/before point in
BUFFER or in a buffer visiting `bibtex-database' if BUFFER is
nil."
  (save-excursion
    (set-buffer (bibtex-helper-buffer buffer))
    (let ((beginning-point (save-excursion
			     (bibtex-beginning-of-entry)
			     (point)))
	  (ending-point (save-excursion
			  (bibtex-end-of-entry)
			  (point))))
      (bibtex-beginning-of-entry)
      (re-search-forward (bibtex-helper-field-regexp field))
      (if (<= (point) ending-point)
	  (buffer-substring-no-properties
	   (save-excursion
	     (bibtex-find-text t nil nil t)
	     (point))
	   (save-excursion
	     (bibtex-find-text nil nil nil t)
	     (point)))
	nil))))

(defun bibtex-helper-strip-baddies (string)
  "Strip stuff that makes BibTeX complain from STRING."
  (replace-regexp-in-string " +" " " (replace-regexp-in-string "\n" " " (replace-regexp-in-string "&" "\\\\&" (replace-regexp-in-string "%" "\\\\%" string)))))

(defun bibtex-helper-add-field (field &optional value buffer no-braces)
  "Add FIELD for the BibTeX record at/before point in BUFFER or
in a buffer visiting `bibtex-database' if BUFFER is nil.  Fill in
the VALUE if given.  If NO-BRACES then don't use {} to delimit
the field (useful for journal abbreviations)."
  (save-excursion
    (set-buffer (bibtex-helper-buffer buffer))
    (bibtex-beginning-of-entry)
    (end-of-line)
    (newline)
    (insert
     (format
      (if no-braces
	  "  %s = %s,"
	"  %s = {%s},")
      (downcase field) (if value (bibtex-helper-strip-baddies value) "")))))
    
(defun bibtex-helper-set-field (field value &optional buffer no-braces)
  "Set FIELD to VALUE for the BibTeX record at/before point in
BUFFER or in a buffer visiting `bibtex-database' if BUFFER is
nil."
  (save-excursion
    (set-buffer (bibtex-helper-buffer buffer))
    (let ((beginning-point (save-excursion
			     (bibtex-beginning-of-entry)
			     (point)))
	  (ending-point (save-excursion
			  (bibtex-end-of-entry)
			  (point))))
    (bibtex-beginning-of-entry)
    (if (re-search-forward (bibtex-helper-field-regexp field) ending-point t)
	(progn
	  (delete-region (save-excursion
			   (bibtex-find-text t nil nil t)
			   (point))
			 (save-excursion
			   (bibtex-find-text nil nil nil t)
			   (point)))
	  (end-of-line)
	  (backward-char 1)
	  (if no-braces
	      (when (looking-back "}")
		(backward-delete-char 2))
	    (backward-char 1))
	  (insert (bibtex-helper-strip-baddies value)))
      (bibtex-helper-add-field field value buffer no-braces)))
    (message (format "%s --> %s" field value))))

(defun bibtex-helper-set-field-interactively (field &optional buffer no-braces)
  "Set FIELD interactively in BUFFER, which defaults in
`bibtex-helper-set-field' to using the value of
`bibtex-database'."
  (interactive)
  (let ((value (if mark-active
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (read-string
		  (format "%s (default %s): "
			  (capitalize field)
			  (word-at-point))
		  nil
		  nil
		  (word-at-point)))))
  (bibtex-helper-set-field field value buffer no-braces)))

(defun bibtex-helper-file-current-buffer ()
  "Rename the file located at `bibtex-helper-source-file' based on
the current active key in the `bibtex-database'"
  (interactive)
  (bibtex-helper-file-paper
   bibtex-helper-source-file
   (bibtex-helper-get-current-key)
   t))

(defun bibtex-helper-complete-buffer ()
  "Complete the current BibTeX entry in `bibtex-database' and call `bibtex-helper-file-current-buffer'."
  (save-excursion
    (set-buffer (bibtex-helper-buffer))
    (bibtex-clean-entry))
  (bibtex-helper-file-current-buffer))

(defun bibtex-helper-view-file (&optional buffer)
  "View the contents of the file property of the BibTeX record
at/before point in BUFFER or `bibtex-database' if not given."
  (interactive)
  (let* ((source-file (bibtex-helper-get-field "file" buffer))
	 (extension (file-name-extension source-file))
	 (viewer-command-string (cadr (assoc extension bibtex-helper-file-view-programs))))
    (when (not viewer-command-string) (error "No viewer program known for files with extension %s" extension))
    (when (not (file-exists-p source-file)) (error "File %s does not exist" source-file))
    (not (call-process (format viewer-command-string source-file) nil 0 nil source-file))))

