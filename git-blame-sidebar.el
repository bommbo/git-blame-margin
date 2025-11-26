;;; git-blame-sidebar.el --- Git blame sidebar like IntelliJ IDEA -*- lexical-binding: t; -*-

;; Author: bommbo
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in a sidebar, similar to IntelliJ IDEA.
;; Shows commit info for each line with color-coded indicators.
;;
;; Usage:
;;   M-x git-blame-sidebar-toggle
;;
;; Command:
;;   git-blame-sidebar-toggle - Toggle sidebar
;;   git-blame-sidebar-copy-commit-hash - Copy commit hash
;;   git-blame-sidebar-show-commit-diff - Show commit diff

;;; Code:

(require 'vc-git)
(require 'cl-lib)

;;;; Custom Variables

(defgroup git-blame-sidebar nil
  "Git blame sidebar display."
  :group 'vc
  :prefix "git-blame-sidebar-")

(defcustom git-blame-sidebar-width 50
  "Width of the blame sidebar window."
  :type 'integer
  :group 'git-blame-sidebar)

(defcustom git-blame-sidebar-format "%h %an %ar"
  "Format string for blame info display.
Available placeholders:
  %h - short hash
  %H - full hash
  %an - author name
  %ae - author email
  %ad - author date
  %ar - author date, relative
  %s - commit subject"
  :type 'string
  :group 'git-blame-sidebar)

(defcustom git-blame-sidebar-position 'left
  "Position of sidebar window."
  :type '(choice (const left) (const right))
  :group 'git-blame-sidebar)

(defcustom git-blame-sidebar-update-delay 0.1
  "Delay (in seconds) before updating sidebar after buffer changes."
  :type 'number
  :group 'git-blame-sidebar)

;;;; Global Variables

(defvar git-blame-sidebar--global-sidebar-buffer nil
  "Global reference to the currently displayed sidebar buffer.")

(defvar git-blame-sidebar--global-source-buffer nil
  "Global reference to the source buffer associated with the sidebar.")

;;;; Buffer-Local Variables

(defvar-local git-blame-sidebar--buffer nil
  "The sidebar buffer associated with this file buffer.")

(defvar-local git-blame-sidebar--blame-data nil
  "Cached blame data: list of (LINE-NUM COMMIT-HASH INFO-STRING COLOR).")

(defvar-local git-blame-sidebar--commit-colors nil
  "Hash table mapping commit hash to color.")

(defvar-local git-blame-sidebar--last-sync-line 0
  "Last synchronized line number to avoid unnecessary updates.")

(defvar-local git-blame-sidebar--update-timer nil
  "Timer for delayed sidebar updates after buffer changes.")

(defvar-local git-blame-sidebar--source-buffer nil
  "The source file buffer for this sidebar.")

;;;; Utility Functions

(defun git-blame-sidebar--get-git-root ()
  "Get git root directory for current buffer."
  (when-let ((file (buffer-file-name)))
	(vc-git-root file)))

(defun git-blame-sidebar--generate-color (commit-hash)
  "Generate a consistent color for COMMIT-HASH."
  (let* ((hash-num (string-to-number (substring commit-hash 0 8) 16))
		 (hue (mod hash-num 360))
		 (saturation 0.6)
		 (lightness (if (eq (frame-parameter nil 'background-mode) 'dark)
						0.5
					  0.7)))
	(git-blame-sidebar--hsl-to-hex hue saturation lightness)))

(defun git-blame-sidebar--hsl-to-hex (h s l)
  "Convert HSL color (H: 0-360, S: 0-1, L: 0-1) to hex string."
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
		 (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1)))))
		 (m (- l (/ c 2.0)))
		 (rgb (cond
			   ((< h 60)  (list c x 0))
			   ((< h 120) (list x c 0))
			   ((< h 180) (list 0 c x))
			   ((< h 240) (list 0 x c))
			   ((< h 300) (list x 0 c))
			   (t         (list c 0 x))))
		 (r (round (* 255 (+ (nth 0 rgb) m))))
		 (g (round (* 255 (+ (nth 1 rgb) m))))
		 (b (round (* 255 (+ (nth 2 rgb) m)))))
	(format "#%02x%02x%02x" r g b)))

(defun git-blame-sidebar--get-commit-color (commit-hash)
  "Get or generate color for COMMIT-HASH."
  (unless git-blame-sidebar--commit-colors
	(setq git-blame-sidebar--commit-colors (make-hash-table :test 'equal)))
  (or (gethash commit-hash git-blame-sidebar--commit-colors)
	  (let ((color (git-blame-sidebar--generate-color commit-hash)))
		(puthash commit-hash color git-blame-sidebar--commit-colors)
		color)))

;;;; Git Blame Functions

(defun git-blame-sidebar--get-commit-info (commit-hash)
  "Get formatted info string for COMMIT-HASH.
Return a default string if git command fails."
  (with-temp-buffer
	(condition-case nil
		(if (zerop (call-process "git" nil t nil "show" "--no-patch"
								 (concat "--format=" git-blame-sidebar-format)
								 commit-hash))
			(string-trim (buffer-string))
		  (format "[Commit %s info unavailable]" (substring commit-hash 0 7)))
	  (error (format "[Error getting info for %s]" (substring commit-hash 0 7))))))

(defun git-blame-sidebar--load-blame-data ()
  "Load git blame data for current buffer.
Return nil if not in a git repository or file not tracked."
  (let* ((file (buffer-file-name))
		 (git-root (git-blame-sidebar--get-git-root)))
	(when (and file git-root)
	  (let ((default-directory git-root)
			(relative-file (file-relative-name file git-root))
			(blame-data nil)
			(current-commit nil)
			(commit-info-cache (make-hash-table :test 'equal))
			(line-num 1))
		(with-temp-buffer
		  (condition-case err
			  (progn
				(when (zerop (call-process "git" nil t nil "blame" "--porcelain" relative-file))
				  (goto-char (point-min))
				  (while (not (eobp))
					(let ((line (buffer-substring-no-properties
								 (line-beginning-position)
								 (line-end-position))))
					  (cond
					   ;; Commit line
					   ((string-match "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" line)
						(setq current-commit (match-string 1 line))
						(let* ((info (or (gethash current-commit commit-info-cache)
										 (let ((info-str (git-blame-sidebar--get-commit-info current-commit)))
										   (puthash current-commit info-str commit-info-cache)
										   info-str)))
							   (color (git-blame-sidebar--get-commit-color current-commit)))
						  (push (list line-num current-commit info color) blame-data)))
					   ;; Code line (starts with tab)
					   ((string-match "^\t" line)
						(setq line-num (1+ line-num)))
					   ;; Ignore other lines
					   (t nil)))
					(forward-line 1))))
			(error
			 (message "Error loading blame data: %s" (error-message-string err))
			 nil)))
		(when blame-data
		  (nreverse blame-data))))))

;;;; Sidebar Buffer Functions

(defun git-blame-sidebar--create-sidebar-buffer (source-buffer)
  "Create sidebar buffer for SOURCE-BUFFER."
  (let* ((source-name (buffer-name source-buffer))
		 (sidebar-name (format "*git-blame: %s*" source-name))
		 (sidebar-buffer (get-buffer-create sidebar-name)))
	(with-current-buffer sidebar-buffer
	  (setq buffer-read-only t
			truncate-lines t)
	  (git-blame-sidebar-mode))
	sidebar-buffer))

(defun git-blame-sidebar--render-sidebar (sidebar-buffer blame-data)
  "Render BLAME-DATA in SIDEBAR-BUFFER."
  (with-current-buffer sidebar-buffer
	(let ((inhibit-read-only t)
		  (max-line (if blame-data (apply #'max (mapcar #'car blame-data)) 0))
		  (line-num 1))
	  (erase-buffer)
	  (dolist (entry blame-data)
		(let* ((entry-line (nth 0 entry))
			   (commit (nth 1 entry))
			   ;; Ensure info is always a string, even if nil
			   (info (or (nth 2 entry) (format "[No info for %s]" (substring commit 0 7))))
			   (color (or (nth 3 entry) "#cccccc"))) ; Fallback color
		  ;; Fill missing lines
		  (while (< line-num entry-line)
			(insert "\n")
			(setq line-num (1+ line-num)))
		  ;; Insert blame info
		  (let ((start (point)))
			(insert (propertize "▌" 'face `(:foreground ,color)))
			(insert " ")
			(insert (propertize info 'face `(:foreground ,color)))
			(add-text-properties start (point)
								 (list 'commit-hash commit
									   'line-number entry-line)))
		  (insert "\n")
		  (setq line-num (1+ line-num))))
	  ;; Fill remaining lines up to max-line
	  (while (<= line-num max-line)
		(insert "\n")
		(setq line-num (1+ line-num)))
	  (goto-char (point-min)))))

(defun git-blame-sidebar--update-sidebar (source-buffer)
  "Update sidebar for SOURCE-BUFFER."
  (when-let* ((blame-data (with-current-buffer source-buffer
							(or git-blame-sidebar--blame-data
								(setq git-blame-sidebar--blame-data
									  (git-blame-sidebar--load-blame-data)))))
			  (sidebar-buffer git-blame-sidebar--global-sidebar-buffer))
	(git-blame-sidebar--render-sidebar sidebar-buffer blame-data)))

(defun git-blame-sidebar--source-post-command ()
  "Hook function for source buffer post-command."
  (when-let* ((sidebar-buffer git-blame-sidebar--global-sidebar-buffer)
			  (sidebar-window (get-buffer-window sidebar-buffer))
			  (current-line (line-number-at-pos))
			  (source-buffer (current-buffer)))
	;; Only update if line number changed
	(when (/= current-line git-blame-sidebar--last-sync-line)
	  (setq git-blame-sidebar--last-sync-line current-line)
	  (let* ((sidebar-line-count (with-current-buffer sidebar-buffer
								   (count-lines (point-min) (point-max))))
			 ;; Target line should not exceed sidebar line count
			 (target-line (min current-line sidebar-line-count)))
		(with-current-buffer sidebar-buffer
		  (goto-char (point-min))
		  (forward-line (max 0 (1- target-line)))
		  (set-window-point sidebar-window (point)))))))

(defun git-blame-sidebar--after-change-function (beg end _)
  "Function to run after buffer changes."
  (when git-blame-sidebar--global-sidebar-buffer
	;; Cancel any pending timer
	(when git-blame-sidebar--update-timer
	  (cancel-timer git-blame-sidebar--update-timer))
	;; Schedule update after delay
	(setq git-blame-sidebar--update-timer
		  (run-with-timer git-blame-sidebar-update-delay nil
						  (lambda (buffer)
							(with-current-buffer buffer
							  (setq git-blame-sidebar--blame-data nil)
							  (git-blame-sidebar--update-sidebar buffer)))
						  (current-buffer)))))

(defun git-blame-sidebar--buffer-switch-hook ()
  "Hook function to update sidebar when switching buffers."
  (when (and git-blame-sidebar--global-sidebar-buffer
			 (buffer-file-name (current-buffer))
			 (git-blame-sidebar--get-git-root)
			 (not (eq (current-buffer) git-blame-sidebar--global-source-buffer)))
	;; Close sidebar for old buffer
	(when (buffer-live-p git-blame-sidebar--global-source-buffer)
	  (with-current-buffer git-blame-sidebar--global-source-buffer
		(remove-hook 'post-command-hook #'git-blame-sidebar--source-post-command t)
		(remove-hook 'after-change-functions #'git-blame-sidebar--after-change-function t)
		(setq git-blame-sidebar--buffer nil)))

	;; Update global references
	(setq git-blame-sidebar--global-source-buffer (current-buffer))

	;; Setup hooks for new buffer
	(with-current-buffer git-blame-sidebar--global-source-buffer
	  (setq git-blame-sidebar--buffer git-blame-sidebar--global-sidebar-buffer)
	  (add-hook 'post-command-hook #'git-blame-sidebar--source-post-command nil t)
	  (add-hook 'after-change-functions #'git-blame-sidebar--after-change-function nil t))

	;; Update sidebar content
	(git-blame-sidebar--update-sidebar git-blame-sidebar--global-source-buffer)
	))

;;;; Window Management

(defun git-blame-sidebar--show-sidebar (source-buffer)
  "Show sidebar for SOURCE-BUFFER."
  (let ((sidebar-buffer (or git-blame-sidebar--global-sidebar-buffer
							(git-blame-sidebar--create-sidebar-buffer source-buffer))))
	;; Update global references
	(setq git-blame-sidebar--global-sidebar-buffer sidebar-buffer
		  git-blame-sidebar--global-source-buffer source-buffer)

	(with-current-buffer source-buffer
	  (setq git-blame-sidebar--buffer sidebar-buffer
			git-blame-sidebar--last-sync-line 0)
	  ;; Add hooks for updates
	  (add-hook 'post-command-hook #'git-blame-sidebar--source-post-command nil t)
	  (add-hook 'after-change-functions #'git-blame-sidebar--after-change-function nil t)
	  (add-hook 'buffer-list-update-hook #'git-blame-sidebar--buffer-switch-hook))

	;; Update sidebar content
	(git-blame-sidebar--update-sidebar source-buffer)

	;; Display sidebar window
	(let ((window (display-buffer-in-side-window
				   sidebar-buffer
				   `((side . ,git-blame-sidebar-position)
					 (window-width . ,git-blame-sidebar-width)
					 (slot . 0)
					 (window-parameters . ((no-other-window . t)))))))
	  (set-window-dedicated-p window t)
	  ;; Initial sync
	  (with-current-buffer source-buffer
		(git-blame-sidebar--source-post-command)))))

(defun git-blame-sidebar--hide-sidebar (source-buffer)
  "Hide sidebar for SOURCE-BUFFER."
  (when-let ((sidebar-buffer git-blame-sidebar--global-sidebar-buffer))
	(when (buffer-live-p sidebar-buffer)
	  (when-let ((window (get-buffer-window sidebar-buffer)))
		(delete-window window))
	  (kill-buffer sidebar-buffer))
	(setq git-blame-sidebar--global-sidebar-buffer nil
		  git-blame-sidebar--global-source-buffer nil)
	(with-current-buffer source-buffer
	  (setq git-blame-sidebar--buffer nil
			git-blame-sidebar--last-sync-line 0
			git-blame-sidebar--blame-data nil
			git-blame-sidebar--commit-colors nil)
	  (remove-hook 'post-command-hook #'git-blame-sidebar--source-post-command t)
	  (remove-hook 'after-change-functions #'git-blame-sidebar--after-change-function t)
	  (remove-hook 'buffer-list-update-hook #'git-blame-sidebar--buffer-switch-hook)
	  (when git-blame-sidebar--update-timer
		(cancel-timer git-blame-sidebar--update-timer)
		(setq git-blame-sidebar--update-timer nil)))))

(defun git-blame-sidebar-copy-commit-hash ()
  "Copy commit hash at current line."
  (interactive)
  (let ((commit (if git-blame-sidebar--source-buffer
					(get-text-property (point) 'commit-hash)
				  (when-let ((entry (cl-find-if (lambda (e) (= (car e) (line-number-at-pos)))
												git-blame-sidebar--blame-data)))
					(nth 1 entry)))))
	(if commit
		(progn
		  (kill-new commit)
		  (message "Copied commit hash: %s" (substring commit 0 8)))
	  (message "No commit at current line"))))

(defun git-blame-sidebar-show-commit-diff ()
  "Show diff of commit at current line."
  (interactive)
  (let ((commit
		 (if (eq major-mode 'git-blame-sidebar-mode)
			 (get-text-property (point) 'commit-hash)
		   (when-let ((entry (cl-find-if (lambda (e) (= (car e) (line-number-at-pos)))
										 git-blame-sidebar--blame-data)))
			 (nth 1 entry)))))
	(if commit
		(let ((buffer-name (format "*Commit Diff: %s*" (substring commit 0 8))))
		  (with-current-buffer (get-buffer-create buffer-name)
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (if (zerop (call-process "git" nil t nil "show" commit))
				  (progn
					(diff-mode)
					(goto-char (point-min))
					(setq buffer-read-only t)
					(pop-to-buffer buffer-name))
				(kill-buffer)
				(message "Failed to show diff for commit %s" commit)))))
	  (message "No commit at current line"))))

;;;; Interactive Commands

;;;###autoload
(defun git-blame-sidebar-toggle ()
  "Toggle git blame sidebar."
  (interactive)
  (if git-blame-sidebar--global-sidebar-buffer
	  (git-blame-sidebar--hide-sidebar (current-buffer))
	(if (git-blame-sidebar--get-git-root)
		(git-blame-sidebar--show-sidebar (current-buffer))
	  (message "Not in a Git repository"))))

(define-derived-mode git-blame-sidebar-mode special-mode "GitBlame"
  "Major mode for git blame sidebar."
  :group 'git-blame-sidebar
  (setq truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 10000))

(provide 'git-blame-sidebar)
;;; git-blame-sidebar.el ends here
