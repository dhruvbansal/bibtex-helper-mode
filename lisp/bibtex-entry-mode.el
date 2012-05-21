(define-derived-mode bibtex-entry-mode text-mode "BibTeX-entry"
  "Major mode for examining text versions of entry documents and inputting information into a BibTeX database."
  (make-variable-buffer-local 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)
  (make-variable-buffer-local 'bibtex-helper-source-file)
  (make-variable-buffer-local 'bibtex-database))

(dolist (field '(author title journal year volume number pages month url abstract keywords address editor chapter institution))
  (let ((name (symbol-name field)))
    (defun
      (make-symbol (concat "bibtex-entry-mode-set-" name))
      (concat "Set " name ". See `bibtex-entry-mode-set-field-interactively' for more.")
      (interactive)
      (bibtex-entry-mode-set-field-interactively name))))

;; shortcuts for field insertion functions in
;; `bibtex-helper-entry-mode'
(define-key bibtex-entry-mode-map "\C-c\C-a"  'bibtex-entry-mode-author)
(define-key bibtex-entry-mode-map "\C-c\C-t"  'bibtex-entry-mode-title)
(define-key bibtex-entry-mode-map "\C-c\C-j"  'bibtex-entry-mode-journal)
(define-key bibtex-entry-mode-map "\C-c\M-j"  'bibtex-helper-abbreviation-for)
(define-key bibtex-entry-mode-map "\C-c\C-y"  'bibtex-entry-mode-year)
(define-key bibtex-entry-mode-map "\C-c\C-v"  'bibtex-entry-mode-volume)
(define-key bibtex-entry-mode-map "\C-c\C-n"  'bibtex-entry-mode-number)
(define-key bibtex-entry-mode-map "\C-c\C-p"  'bibtex-entry-mode-pages)
(define-key bibtex-entry-mode-map "\C-c\C-\m" 'bibtex-entry-mode-month)
(define-key bibtex-entry-mode-map "\C-c\C-u"  'bibtex-entry-mode-url)
(define-key bibtex-entry-mode-map "\C-c\C-b"  'bibtex-entry-mode-abstract)
(define-key bibtex-entry-mode-map "\C-c\C-k"  'bibtex-entry-mode-keywords)
(define-key bibtex-entry-mode-map "\C-c\C-l"  'bibtex-entry-mode-address)
(define-key bibtex-entry-mode-map "\C-c\C-d"  'bibtex-entry-mode-editor)
(define-key bibtex-entry-mode-map "\C-c\C-i"  'bibtex-entry-mode-institution)

;; non-field-insertion shortcuts for `bibtex-entry-mode'
(define-key bibtex-entry-mode-map "\C-c\C-f" 'bibtex-entry-mode-file-current-buffer)
(define-key bibtex-entry-mode-map "\C-c\C-c" 'bibtex-entry-mode-complete-buffer)

(defun bibtex-entry-mode-from-file (&optional path)
  "Open PATH in a text buffer (regardless of the actual source
file's type) in `bibtex-entry-mode'."
  (interactive)
  (let*
      ((source-filename			; find the original file
	(cond
	 (path path)			; given explicitly
	 
	 ((and				    ; from the current item's
					    ; file property if in a
					    ; bibtex buffer
	   (memq major-mode '(bibtex-mode)) 
	   (bibtex-helper-get-field "file")
	   (not
	    (string= (bibtex-helper-get-field "file") "")))
	  (bibtex-helper-get-field "file"))
	 
	 (t				; prompt for filename
	  (read-file-name "Original File: " nil nil t))))

       (text-buffer (generate-new-buffer
		     (file-name-nondirectory source-filename)))

       (extension (file-name-extension source-filename))

       (viewer-format-string (cadr (assoc extension bibtex-helper-file-translation-programs))))
    (if viewer-format-string
	(shell-command (format viewer-format-string source-filename) text-buffer)
      (error "Can't process file extension %s" extension))
    (switch-to-buffer text-buffer)
    (delete-other-windows)
    (bibtex-helper-entry-mode)
    (mapcar
     (lambda (bug)
       (save-excursion 
	 (replace-string (car bug) (car (cdr bug)))))
     bibtex-helper-translation-bugs)
    (setq bibtex-helper-source-file source-filename)))
