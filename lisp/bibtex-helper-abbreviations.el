(defvar bibtex-journal-long-abbrev-file "/home/dhruv/projects/work/papers/abbreviations-long.bib" "A BibTeX file containing long-form journal abbreviations.")
(defvar bibtex-journal-short-abbrev-file "/home/dhruv/projects/work/papers/abbreviations-short.bib" "BibTeX file containing short-form journal abbreviations.")
(defvar bibtex-journal-abbrev-regexp "^@string(\\([-a-zA-Z0-9_]+\\) = \"\\([^\"]+\\)\")" "Regular expression which captures the abbreviation and content of a BibTeX @string declaration")

(defun bibtex-helper-abbreviation-expand (&optional abbrev short-name strip-periods)
  "Return the name of the journal corresponding to ABBREV (or, if
SHORT-NAME, then its shortened abbreviation).  If ABBREV is not
specified, then either the region or the word at point is used.
If STRIP-PERIODS then remove any periods from the journal name
before returning."
  (interactive)
  (when (not abbrev)
    (setq abbrev (if mark-active
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (word-at-point))))
  (save-window-excursion
    (find-file (if short-name bibtex-journal-short-abbrev-file bibtex-journal-long-abbrev-file))
    (beginning-of-buffer)
    (if (re-search-forward (concat "^@string(" abbrev " = ") nil t)
	(let* ((line (line-content))
	       (journal (progn
			  (string-match bibtex-journal-abbrev-regexp line)
			  (match-string 2 line))))
	  (if strip-periods
	      (replace-regexp-in-string "\\." "" journal)
	  journal))
      nil)))

(defun bibtex-helper-abbreviation-explain ()
  (interactive)
  (save-excursion
    (re-search-forward "^  journal =")
    (end-of-line)
    (backward-char 1)
    (message (concat (word-at-point) ": " (bibtex-helper-abbreviation-expand)))))

(defun bibtex-helper-abbreviation-search (abbrev-path journal preserve-point)
  (save-window-excursion
    (find-file abbrev-path)
    (unless preserve-point (beginning-of-buffer))
    (let ((line nil))
      (when (re-search-forward journal nil t)
	(setq line (line-content))
	(string-match bibtex-journal-abbrev-regexp line)
	(message "%s => %s" (match-string 1 line) (match-string 2 line))
	(list (match-string 1 line)     ; abbrev
	      (match-string 2 line)))))) ; long/short name

(defun bibtex-helper-abbreviation-for (&optional journal return-only)
  "Insert the abbreviation for JOURNAL at point and flash the
full journal name in the minibuffer .  With repeated calls, use
the next match for JOURNAL in `bibtex-abbrev-file'.  If
RETURN-ONLY then merely return the result instead of inserting
it."
  (interactive)
  (when (eq this-command last-command)
    (backward-kill-word 1)
    (backward-delete-char 1))
  (unless journal
    (setq journal (if mark-active
		      (buffer-substring-no-properties (region-beginning) (region-end))
		    (word-at-point))))

  (let ((match-data nil))
    (setq match-data
	  (or
	   ; search long names
	   (bibtex-helper-abbreviation-search
	    bibtex-journal-long-abbrev-file
	    journal
	    (eq this-command last-command))
	   ; search short names
	   (bibtex-helper-abbreviation-search
	    bibtex-journal-short-abbrev-file
	    journal
	    (eq this-command last-command))))
    (if match-data
	(unless return-only
	  (progn
	    (insert " ")
	    (insert (elt match-data 0))
	    (message (elt match-data 1)))
	  (elt match-data 0))
      (message (format "No match for %s" journal)))))

(defun bibtex-helper-abbreviation-exists-p (abbrev)
  "Does ABBREV already occur in `bibtex-abbrev-file'?"
  (save-window-excursion
    (find-file bibtex-journal-long-abbrev-file)
    (beginning-of-buffer)
    (re-search-forward (concat "^@string(" abbrev " = ") nil t)))

(defun bibtex-helper-abbreviation-new (abbrev long-name short-name)
  "Insert a new journal abbreviation ABBREV corresponding to
LONG-NAME with the short-form SHORT-NAME.  Prompt for any which
are not given."
  (interactive "U\nU\nU")
  (unless long-name
    (setq long-name (read-string "Journal Long Name: ")))
  (unless short-name
    (setq short-name (read-string (format "Journal Short Name (%s): " long-name))))
  (unless abbrev
    (setq abbrev (read-string (format "Abbreviation (%s): " long-name))))
  (while (bibtex-helper-abbreviation-exists-p abbrev)
    (setq abbrev (read-string (format "Abbreviation %s already exists.  New abbreviation (%s): " abbrev long-name))))
  (save-window-excursion
    (find-file bibtex-journal-long-abbrev-file)
    (end-of-buffer)
    (unless (looking-at "^") (newline))
    (insert (format "@string(%s = \"%s\")" abbrev long-name))
    (newline)
    (find-file bibtex-journal-short-abbrev-file)
    (end-of-buffer)
    (unless (looking-at "^") (newline))
    (insert (format "@string(%s = \"%s\")" abbrev short-name))))

(provide 'bibtex-helper-abbreviations)
