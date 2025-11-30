;;; git-blame-margin.el --- Git blame in left margin -*- lexical-binding: t; -*-

;; Author: bommbo
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, tools

;;; Commentary:
;; Displays git blame info in the left margin using text properties (`line-prefix`).
;; Retains full logic from v6.0: global toggle, auto-follow, edit-hide, save-reload,
;; lazy loading, async parsing, and scroll-aware rendering — but with better performance.

;;; Code:

(require 'vc-git)
(require 'cl-lib)

;;;; Customization

(defgroup git-blame-margin nil
  "Git blame in left margin."
  :group 'vc
  :prefix "git-blame-margin-")

(defcustom git-blame-margin-width 40
  "Width of the left margin in characters."
  :type 'integer
  :group 'git-blame-margin)

(defcustom git-blame-margin-lazy-threshold 3000
  "File size threshold (lines) for lazy loading."
  :type 'integer
  :group 'git-blame-margin)

(defcustom git-blame-margin-visible-radius 200
  "Number of context lines around visible window for initial load."
  :type 'integer
  :group 'git-blame-margin)

;;;; Global State

(defvar git-blame-margin--global-enabled nil
  "Global toggle state for all git buffers.")

;;;; Cache State

(defvar-local git-blame-margin--cache nil
  "Hash table: line-number -> (commit-hash author date summary).")

(defvar-local git-blame-margin--cache-complete-p nil
  "t if entire file cached.")

(defvar-local git-blame-margin--commit-colors nil
  "Hash table: commit-hash -> color.")

(defvar-local git-blame-margin--enabled-p nil
  "t if blame is enabled for this buffer.")

(defvar-local git-blame-margin--currently-visible-p nil
  "t if margin is currently visible (not hidden due to editing).")

;;;; Async Process

(defvar-local git-blame-margin--blame-process nil
  "Running async git blame process.")

;;;; Scroll Handler

(defvar-local git-blame-margin--scroll-timer nil)

;;;; Color

(defun git-blame-margin--hash-to-color (commit-hash)
  "Generate color from COMMIT-HASH."
  (let* ((hash-num (string-to-number (substring commit-hash 0 8) 16))
		 (hue (mod hash-num 360))
		 (saturation 0.6)
		 (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
		 (lightness (if is-dark 0.7 0.4)))
	(let* ((c (* (- 1 (abs (- (* 2 lightness) 1))) saturation))
		   (x (* c (- 1 (abs (- (mod (/ hue 60.0) 2) 1)))))
		   (m (- lightness (/ c 2.0)))
		   (rgb (cond
				 ((< hue 60)  (list c x 0))
				 ((< hue 120) (list x c 0))
				 ((< hue 180) (list 0 c x))
				 ((< hue 240) (list 0 x c))
				 ((< hue 300) (list x 0 c))
				 (t           (list c 0 x))))
		   (r (round (* 255 (+ (nth 0 rgb) m))))
		   (g (round (* 255 (+ (nth 1 rgb) m))))
		   (b (round (* 255 (+ (nth 2 rgb) m)))))
	  (format "#%02x%02x%02x" r g b))))

(defun git-blame-margin--get-commit-color (commit-hash)
  "Get or generate color for COMMIT-HASH."
  (unless git-blame-margin--commit-colors
	(setq git-blame-margin--commit-colors (make-hash-table :test 'equal)))
  (or (gethash commit-hash git-blame-margin--commit-colors)
	  (let ((color (git-blame-margin--hash-to-color commit-hash)))
		(puthash commit-hash color git-blame-margin--commit-colors)
		color)))

;;;; Cache

(defun git-blame-margin--cache-set (line-num commit-hash author date summary)
  (unless git-blame-margin--cache
	(setq git-blame-margin--cache (make-hash-table :test 'equal)))
  (puthash line-num (list commit-hash author date summary) git-blame-margin--cache))

(defun git-blame-margin--cache-get (line-num)
  (when git-blame-margin--cache
	(gethash line-num git-blame-margin--cache)))

;;;; Parse - Streaming Friendly

(defun git-blame-margin--parse-porcelain (output buffer)
  "Parse git blame porcelain OUTPUT and populate cache in BUFFER."
  (with-temp-buffer
	(insert output)
	(goto-char (point-min))
	(let ((current-commit nil)
		  (current-line nil)
		  (current-author nil)
		  (current-date nil)
		  (current-summary nil)
		  (commit-cache (make-hash-table :test 'equal))
		  (count 0))
	  (while (not (eobp))
		(cond
		 ;; Commit header: HASH original-line final-line [num-lines]
		 ((looking-at "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)\\(?: \\([0-9]+\\)\\)?")
		  (setq current-commit (match-string 1)
				current-line (string-to-number (match-string 3)))
		  (let ((cached (gethash current-commit commit-cache)))
			(if cached
				(setq current-author (nth 0 cached)
					  current-date (nth 1 cached)
					  current-summary (nth 2 cached))
			  (setq current-author nil current-date nil current-summary nil))))

		 ((looking-at "^author \\(.+\\)")
		  (setq current-author (match-string 1)))

		 ((looking-at "^author-time \\([0-9]+\\)")
		  (setq current-date (format-time-string "%Y-%m-%d"
												 (seconds-to-time (string-to-number (match-string 1))))))

		 ((looking-at "^summary \\(.+\\)")
		  (setq current-summary (match-string 1))
		  (puthash current-commit
				   (list current-author current-date current-summary)
				   commit-cache))

		 ;; Code line (starts with tab) - only cache once per commit header
		 ((looking-at "^\t")
		  (when (and current-commit current-line)
			(with-current-buffer buffer
			  (git-blame-margin--cache-set
			   current-line current-commit
			   (or current-author "Unknown")
			   (or current-date "?")
			   (or current-summary "")))
			(setq count (1+ count))
			;; Don't auto-increment - wait for next commit header
			(setq current-commit nil
				  current-line nil))))
		(forward-line 1))
	  count)))

;;;; Visible Range

(defun git-blame-margin--get-visible-range ()
  "Get (START . END) line range to load."
  (let* ((win (get-buffer-window))
		 (start-pos (window-start win))
		 (end-pos (window-end win t))
		 (start-line (line-number-at-pos start-pos))
		 (end-line (line-number-at-pos end-pos))
		 (margin git-blame-margin-visible-radius))
	(cons (max 1 (- start-line margin))
		  (min (line-number-at-pos (point-max))
			   (+ end-line margin)))))

(defun git-blame-margin--should-use-lazy-p ()
  "Check if file is large enough for lazy loading."
  (> (line-number-at-pos (point-max))
	 git-blame-margin-lazy-threshold))

;;;; Async Blame Process

(defun git-blame-margin--start-blame-process (buffer &optional start end)
  "Start async git blame process for BUFFER with optional line range."
  (with-current-buffer buffer
	;; Kill existing process
	(when (process-live-p git-blame-margin--blame-process)
	  (delete-process git-blame-margin--blame-process)
	  (setq git-blame-margin--blame-process nil))

	(let* ((file (buffer-file-name))
		   (git-root (vc-git-root file))
		   (relative-file (file-relative-name file git-root))
		   (default-directory git-root)
		   (cmd (append (list "git" "blame" "--porcelain")
						(when (and start end)
						  (list (format "-L%d,%d" start end)))
						(list relative-file)))
		   (process-connection-type nil)
		   (proc-buffer (generate-new-buffer " *git-blame-margin*")))

	  (condition-case err
		  (let ((proc (apply #'start-process
							 "git-blame-margin"
							 proc-buffer
							 cmd)))

			(set-process-query-on-exit-flag proc nil)
			(setq git-blame-margin--blame-process proc)
			(process-put proc 'source-buffer buffer)
			(process-put proc 'is-partial (and start end))

			(set-process-sentinel
			 proc
			 (lambda (p event)
			   (when (memq (process-status p) '(exit signal))
				 (let* ((exit-code (process-exit-status p))
						(output (with-current-buffer (process-buffer p)
								  (buffer-string)))
						(source-buf (process-get p 'source-buffer))
						(is-partial (process-get p 'is-partial)))

				   (when (and (buffer-live-p source-buf)
							  (= exit-code 0)
							  (> (length output) 100))
					 (with-current-buffer source-buf
					   ;; Check if still enabled before rendering
					   (when (and git-blame-margin--enabled-p
								  git-blame-margin--global-enabled)
						 (let ((count (git-blame-margin--parse-porcelain output source-buf)))
						   (unless is-partial
							 (setq git-blame-margin--cache-complete-p t))
						   (git-blame-margin--render-visible)
						   (message "Git blame: loaded %d lines%s"
									count
									(if is-partial " (partial)" ""))))))

				   (when (buffer-live-p (process-buffer p))
					 (kill-buffer (process-buffer p)))

				   (when (buffer-live-p source-buf)
					 (with-current-buffer source-buf
					   (setq git-blame-margin--blame-process nil)))))))

			(minibuffer-message
			 (format "Git blame: loading%s..."
					 (if (and start end)
						 (format " lines %d-%d" start end)
					   " entire file"))))

		(error
		 (message "Git blame error: %s" err)
		 (when (buffer-live-p proc-buffer)
		   (kill-buffer proc-buffer))
		 nil)))))

;;;; Loading Strategy

(defun git-blame-margin--load-initial ()
  "Load blame data. Always start with visible area, then full in background."
  (let* ((range (git-blame-margin--get-visible-range))
		 (start (car range))
		 (end (cdr range)))
	(git-blame-margin--start-blame-process (current-buffer) start end)
	(run-with-idle-timer
	 0.5 nil
	 (lambda (buf)
	   (when (and (buffer-live-p buf)
				  (with-current-buffer buf
					;; Check if still enabled before loading full file
					(and git-blame-margin--enabled-p
						 git-blame-margin--global-enabled
						 (not git-blame-margin--cache-complete-p))))
		 (with-current-buffer buf
		   (minibuffer-message "Git blame: loading full file in background...")
		   (git-blame-margin--start-blame-process buf nil nil))))
	 (current-buffer))))

;;;; Show/Hide Helpers

(defun git-blame-margin--clear-display ()
  "Clear margin display by removing text properties."
  (let ((inhibit-read-only t)
		(inhibit-modification-hooks t)
		
		(was-modified (buffer-modified-p)))
	(save-excursion
	  (save-restriction
		(widen)
		(remove-text-properties (point-min) (point-max)
								'(line-prefix nil))))
	
	(set-buffer-modified-p was-modified)))

(defun git-blame-margin--hide-display ()
  "Hide the margin display (but keep enabled state)."
  (when git-blame-margin--currently-visible-p
	(git-blame-margin--clear-display)
	(setq left-margin-width 0)
	(set-window-buffer (selected-window) (current-buffer))
	(setq git-blame-margin--currently-visible-p nil)))

(defun git-blame-margin--show-display ()
  "Show the margin display if enabled."
  (when git-blame-margin--enabled-p
	(setq left-margin-width git-blame-margin-width)
	(set-window-buffer (selected-window) (current-buffer))
	(git-blame-margin--render-visible)
	(setq git-blame-margin--currently-visible-p t)))

;;;; Buffer Switch Detection

(defvar git-blame-margin--buffer-switch-timer nil
  "Timer for debouncing buffer switches.")

(defvar git-blame-margin--last-buffer nil
  "Track last buffer to detect switches.")

(defun git-blame-margin--on-buffer-switch ()
  "Handle buffer switch - auto-enable blame for git files if globally enabled."
  (let ((current-buf (current-buffer)))
	(when (and (not (eq current-buf git-blame-margin--last-buffer))
			   (buffer-live-p current-buf)
			   git-blame-margin--global-enabled)
	  (setq git-blame-margin--last-buffer current-buf)
	  (when git-blame-margin--buffer-switch-timer
		(cancel-timer git-blame-margin--buffer-switch-timer))
	  (setq git-blame-margin--buffer-switch-timer
			(run-with-idle-timer
			 0.05 nil
			 (lambda (buf)
			   (when (and (buffer-live-p buf)
						  (eq buf (current-buffer)))
				 (with-current-buffer buf
				   ;; Check global toggle before auto-enabling
				   (when git-blame-margin--global-enabled
					 ;; Auto-enable for git files if not already enabled
					 (when (and (buffer-file-name)
								(vc-git-registered (buffer-file-name))
								(not git-blame-margin--enabled-p)
								(not (string-prefix-p " " (buffer-name)))
								(not (string-prefix-p "*" (buffer-name))))
					   ;; Load full file when switching, not just visible range
					   (setq git-blame-margin--enabled-p t)
					   (git-blame-margin--show-display)
					   (add-hook 'after-change-functions #'git-blame-margin--on-buffer-change nil t)
					   (add-hook 'window-scroll-functions #'git-blame-margin--on-scroll nil t)
					   (add-hook 'after-save-hook #'git-blame-margin--on-after-save nil t)
					   ;; Always load full file on switch, ignoring lazy threshold
					   (git-blame-margin--start-blame-process buf nil nil))
					 ;; Restore display if already enabled but hidden
					 (when (and git-blame-margin--enabled-p
								(not (buffer-modified-p))
								(not git-blame-margin--currently-visible-p))
					   (git-blame-margin--show-display))))))
			 current-buf)))))

;;;; Edit Detection

(defun git-blame-margin--on-buffer-change (beg end prev-len)
  "Hide margin when buffer is modified; auto-show if becomes clean (e.g., after undo)."
  (when git-blame-margin--enabled-p
	(if (buffer-modified-p)
		(when git-blame-margin--currently-visible-p
		  (git-blame-margin--hide-display)
		  (minibuffer-message "Git blame: hidden (editing)"))
	  (unless git-blame-margin--currently-visible-p
		(git-blame-margin--show-display)))))

(defun git-blame-margin--on-after-save ()
  "Reload and show margin after save."
  (when git-blame-margin--enabled-p
	(setq git-blame-margin--cache nil
		  git-blame-margin--cache-complete-p nil
		  git-blame-margin--commit-colors nil)
	(when (process-live-p git-blame-margin--blame-process)
	  (delete-process git-blame-margin--blame-process)
	  (setq git-blame-margin--blame-process nil))
	(git-blame-margin--show-display)
	(git-blame-margin--load-initial)
	(minibuffer-message "Git blame: reloaded")))

;;;; Render with Text Properties

(defun git-blame-margin--create-margin-text (commit-hash author date summary)
  "Create formatted margin text with color."
  (let* ((color (git-blame-margin--get-commit-color commit-hash))
		 (short-hash (substring commit-hash 0 7))
		 (text (format "%s %s %s" short-hash date author))
		 (truncated (truncate-string-to-width
					 text git-blame-margin-width nil ?\s)))
	(propertize truncated 'face `(:foreground ,color))))

(defun git-blame-margin--render-visible ()
  "Render blame info for visible lines using text properties."
  (git-blame-margin--clear-display)

  (when git-blame-margin--cache
	(let* ((range (git-blame-margin--get-visible-range))
		   (start-line (car range))
		   (end-line (cdr range))
		   (count 0)
		   (inhibit-read-only t)
		   (inhibit-modification-hooks t)
		   (was-modified (buffer-modified-p)))

	  (save-excursion
		(goto-char (point-min))
		(forward-line (1- start-line))

		(cl-loop for line from start-line to end-line
				 while (not (eobp))
				 do (progn
					  (when-let ((data (git-blame-margin--cache-get line)))
						(let* ((pos (line-beginning-position))
							   (margin-text (apply #'git-blame-margin--create-margin-text data)))
						  (put-text-property
						   pos (1+ pos) 'line-prefix
						   (propertize " " 'display
									   `((margin left-margin) ,margin-text)))
						  (setq count (1+ count))))
					  (forward-line 1))))

	  (set-buffer-modified-p was-modified)

	  (when (> count 0)
		(minibuffer-message (format "Git blame: rendered %d lines" count))))))

;;;; Scroll Handler

(defun git-blame-margin--on-scroll (&rest _)
  "Handle scroll events with debouncing."
  (when git-blame-margin--scroll-timer
	(cancel-timer git-blame-margin--scroll-timer))
  (setq git-blame-margin--scroll-timer
		(run-with-idle-timer
		 0.1 nil
		 (lambda (buf)
		   (when (and (buffer-live-p buf)
					  (with-current-buffer buf
						;; Check if still enabled before rendering
						(and git-blame-margin--enabled-p
							 git-blame-margin--global-enabled)))
			 (with-current-buffer buf
			   (when git-blame-margin--cache
				 (git-blame-margin--render-visible)))))
		 (current-buffer))))

;;;; Commands

;;;###autoload
(defun git-blame-margin-reload ()
  "Reload git blame data."
  (interactive)
  (when git-blame-margin--enabled-p
	(setq git-blame-margin--cache nil
		  git-blame-margin--cache-complete-p nil
		  git-blame-margin--commit-colors nil)
	(git-blame-margin--clear-display)
	(when (process-live-p git-blame-margin--blame-process)
	  (delete-process git-blame-margin--blame-process)
	  (setq git-blame-margin--blame-process nil))
	(git-blame-margin--show-display)
	(git-blame-margin--load-initial)))

;;;###autoload
(defun git-blame-margin-show-cache-info ()
  "Show cache statistics."
  (interactive)
  (if (not git-blame-margin--cache)
	  (message "No cache")
	(let ((total (line-number-at-pos (point-max)))
		  (cached (hash-table-count git-blame-margin--cache)))
	  (message "Cache: %d/%d lines (%.1f%%)%s"
			   cached total (* 100.0 (/ (float cached) total))
			   (if git-blame-margin--cache-complete-p " [complete]" "")))))

;;;; Toggle Command

(defun git-blame-margin--enable-for-buffer ()
  "Enable blame for current buffer (internal function)."
  (setq git-blame-margin--enabled-p t)
  (git-blame-margin--show-display)
  (git-blame-margin--load-initial)
  (add-hook 'after-change-functions #'git-blame-margin--on-buffer-change nil t)
  (add-hook 'window-scroll-functions #'git-blame-margin--on-scroll nil t)
  (add-hook 'after-save-hook #'git-blame-margin--on-after-save nil t))

(defun git-blame-margin--disable-for-buffer ()
  "Disable blame for current buffer (internal function)."
  (git-blame-margin--hide-display)
  (remove-hook 'after-change-functions #'git-blame-margin--on-buffer-change t)
  (remove-hook 'window-scroll-functions #'git-blame-margin--on-scroll t)
  (remove-hook 'after-save-hook #'git-blame-margin--on-after-save t)
  (when git-blame-margin--scroll-timer
	(cancel-timer git-blame-margin--scroll-timer)
	(setq git-blame-margin--scroll-timer nil))
  (when (process-live-p git-blame-margin--blame-process)
	(delete-process git-blame-margin--blame-process)
	(setq git-blame-margin--blame-process nil))
  (setq git-blame-margin--cache nil
		git-blame-margin--cache-complete-p nil
		git-blame-margin--commit-colors nil
		git-blame-margin--enabled-p nil
		git-blame-margin--currently-visible-p nil))

;;;###autoload
(defun git-blame-margin-toggle ()
  "Toggle git blame margin with auto-follow."
  (interactive)
  (if git-blame-margin--global-enabled
	  ;; OFF globally
	  (progn
		;; 1. Set global flag to nil first
		(setq git-blame-margin--global-enabled nil)
		;; 2. Remove global hooks immediately
		(remove-hook 'buffer-list-update-hook #'git-blame-margin--on-buffer-switch)
		(remove-hook 'window-configuration-change-hook #'git-blame-margin--on-buffer-switch)
		;; 3. Cancel buffer-switch timer
		(when git-blame-margin--buffer-switch-timer
		  (cancel-timer git-blame-margin--buffer-switch-timer)
		  (setq git-blame-margin--buffer-switch-timer nil))
		;; 4. Disable blame for all buffers
		(dolist (buf (buffer-list))
		  (when (buffer-live-p buf)
			(with-current-buffer buf
			  (when git-blame-margin--enabled-p
				(git-blame-margin--disable-for-buffer)))))
		(message "Git blame: OFF (globally)"))
	;; ON globally
	(if (not (and (buffer-file-name)
				  (vc-git-registered (buffer-file-name))))
		(message "Not a git file")
	  (setq git-blame-margin--global-enabled t)
	  (git-blame-margin--enable-for-buffer)
	  (add-hook 'buffer-list-update-hook #'git-blame-margin--on-buffer-switch)
	  (add-hook 'window-configuration-change-hook #'git-blame-margin--on-buffer-switch)
	  (message "Git blame: ON (globally)"))))

(provide 'git-blame-margin)
;;; git-blame-margin.el ends here
