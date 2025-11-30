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

(provide 'git-blame-margin-extensions)
;;; git-blame-margin-extensions.el ends here
