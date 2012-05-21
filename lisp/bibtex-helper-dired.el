;; dired
(require 'dired)
(defun dired/bibtex-ready-source-for-filing ()
  "Ready the filename at point for filing into BibTex using
`bibtex-helper-ready-source-for-filing'."
  (interactive)
  (bibtex-helper-ready-source-for-filing
   (path default-directory
	 (let ((beg (point)))
	   (save-excursion
	     (end-of-line)
	     (buffer-substring-no-properties beg (point)))))))
(define-key dired-mode-map "\C-c\C-f" 'dired/bibtex-ready-source-for-filing)
