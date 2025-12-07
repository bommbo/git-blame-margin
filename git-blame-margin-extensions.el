;;; git-blame-margin-extensions.el --- Extensions for git-blame-margin -*- lexical-binding: t; -*-

;; Author: bommbo
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (git-blame-margin "6.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Provides `git-blame-margin-copy-commit-hash` and `git-blame-margin-show-commit-diff`
;; that work with `git-blame-margin.el` using its cache.

;;; Code:

(require 'git-blame-margin)
(require 'vc-git)

;;;; Helper: Get git root directory

(defun git-blame-margin--get-git-root ()
  "Get git root directory for current buffer."
  (when-let ((file (buffer-file-name)))
	(vc-git-root file)))

;;;; Helper: Get commit hash for current line from git-blame-margin cache

(defun git-blame-margin--get-commit-hash-at-line (line-num)
  "Get commit hash for LINE-NUM from git-blame-margin cache.
Returns the 40-char commit hash, or nil if not available."
  (when-let ((data (git-blame-margin--cache-get line-num)))
	(nth 0 data)))  ; (commit-hash author date summary)

(defun git-blame-margin--get-current-commit-hash ()
  "Get commit hash for current line in current buffer.
Relies on git-blame-margin cache. Returns full hash or nil."
  (let ((line-num (line-number-at-pos)))
	(git-blame-margin--get-commit-hash-at-line line-num)))

;;;; Command: Copy commit hash

;;;###autoload
(defun git-blame-margin-copy-commit-hash ()
  "Copy the Git commit hash for the current line to the kill ring.
Relies on git-blame-margin being enabled and cache populated."
  (interactive)
  (let ((commit (git-blame-margin--get-current-commit-hash)))
	(if commit
		(progn
		  (kill-new commit)
		  (message "Copied commit hash: %s" (substring commit 0 8)))
	  (message "No Git blame data available for current line (margin not loaded?)"))))

;;;; Command: Show commit diff

;;;###autoload
(defun git-blame-margin-show-commit-diff ()
  "Show the diff (git show) of the commit responsible for the current line.
Relies on git-blame-margin being enabled and cache populated."
  (interactive)
  (let ((commit (git-blame-margin--get-current-commit-hash)))
	(if (and commit (git-blame-margin--get-git-root))
		(let ((buffer-name (format "*Commit Diff: %s*" (substring commit 0 8))))
		  (with-current-buffer (get-buffer-create buffer-name)
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (if (zerop (call-process "git" nil t nil "show" commit))
				  (progn
					(diff-mode)
					(goto-char (point-min))
					(setq buffer-read-only t)
					(view-mode 1)
					(pop-to-buffer buffer-name))
				(progn
				  (kill-buffer)
				  (message "Failed to run `git show %s`" (substring commit 0 8)))))))
	  (message "No Git blame data available for current line (margin not loaded?)"))))

;;;###autoload
(defun git-blame-margin-jump ()
  "Show timeline and jump to selected line."
  (interactive)
  (unless git-blame-margin--cache
    (user-error "Git blame data not loaded"))
  
  (let ((date-to-line (make-hash-table :test 'equal))
        choices)
    
    ;; Collect minimum line number for each date
    (maphash
     (lambda (line-num data)
       (let* ((date (nth 2 data))
              (existing-line (gethash date date-to-line)))
         (when (or (not existing-line) (< line-num existing-line))
           (puthash date line-num date-to-line))))
     git-blame-margin--cache)
    
    ;; Convert hash to list and format display
    (maphash
     (lambda (date line-num)
       (let* ((data (git-blame-margin--cache-get line-num))
              (commit-hash (nth 0 data))
              (author (nth 1 data))
              (summary (nth 3 data))
              (short-hash (substring commit-hash 0 7))
              (display (format "%s %s L%-5d %s %s"
                               short-hash date line-num author summary)))
         (push (list date display line-num) choices)))
     date-to-line)
    
    ;; Sort by date field (newest first)
    (setq choices (sort choices (lambda (a b) 
                                  (string> (nth 0 a) (nth 0 b)))))
    
    ;; Build alist: (display . line-num)
    (setq choices (mapcar (lambda (x) 
                            (cons (nth 1 x) (nth 2 x)))
                          choices))
    
    (if (not choices)
        (user-error "No data available")
      (let* ((display-list (mapcar #'car choices))
             (completion-styles '(basic))
             (vertico-sort-function nil)
             (selection (let ((completing-read-function #'completing-read-default))
                          (completing-read "Jump to: " display-list nil t)))
             (line-num (cdr (assoc selection choices))))
        (goto-char (point-min))
        (forward-line (1- line-num))
        (recenter)
        (message "Line %d" line-num)))))

(defun git-blame-margin--jump-to-commit-line (direction)
  "Jump to another line in the same commit.
DIRECTION should be 1 for next, -1 for previous."
  (interactive)
  (let* ((line (line-number-at-pos))
         (entry (git-blame-margin--cache-get line)))
    (unless entry
      (user-error "No blame info at this line"))
    (let* ((commit (car entry))
           (lines '()))

      ;; Collect all lines for the same commit
      (maphash (lambda (ln data)
                 (when (equal (car data) commit)
                   (push ln lines)))
               git-blame-margin--cache)

      (setq lines (sort lines #'<))

      ;; Find next/prev
      (let* ((pos (cl-position line lines))
             (target
              (if (null pos)
                  (user-error "Current line not found in commit list")
                (nth (+ pos direction) lines))))
        (if target
            (goto-char (point-min))
          (user-error "No more lines in this commit"))
        (forward-line (1- target))
        (message "Jumped to line %d (commit %s)" target commit)))))

(defun git-blame-margin-next-commit-line ()
  (interactive)
  (git-blame-margin--jump-to-commit-line 1))

(defun git-blame-margin-prev-commit-line ()
  (interactive)
  (git-blame-margin--jump-to-commit-line -1))

(provide 'git-blame-margin-extensions)
;;; git-blame-margin-extensions.el ends here
