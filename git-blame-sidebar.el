;;; git-blame-sidebar.el --- Git blame sidebar like IntelliJ IDEA (robust) -*- lexical-binding: t; -*-

;; Author: bommbo
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Displays Git blame info for the current line, with support for auto-following the active Git buffer.

;;; Code:

(require 'vc-git)
(require 'cl-lib)
(require 'subr-x)

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

(defcustom git-blame-sidebar-async t
  "When non-nil, run git blame and git show asynchronously."
  :type 'boolean
  :group 'git-blame-sidebar)

(defcustom git-blame-sidebar-visible-radius 200
  "Number of context lines to include around the visible window when blaming.
Set to 0 to blame only the visible window. Increase for more prefetching."
  :type 'integer
  :group 'git-blame-sidebar)

(defcustom git-blame-sidebar-max-lines 100000
  "Maximum number of lines to process for blame data. Prevents memory issues with very large files."
  :type 'integer
  :group 'git-blame-sidebar)

;;;; Global Variables

(defvar git-blame-sidebar--global-sidebar-buffer nil
  "Global reference to the currently displayed sidebar buffer.")

(defvar git-blame-sidebar--global-source-buffer nil
  "Global reference to the source buffer associated with the sidebar.")

(defvar git-blame-sidebar--global-follow-timer nil
  "Timer to debounce buffer switch due to window selection.")

;;;; Buffer-Local Variables

(defvar-local git-blame-sidebar--buffer nil
  "The sidebar buffer associated with this file buffer.")

(defvar-local git-blame-sidebar--blame-data nil
  "Cached blame  list of (LINE-NUM COMMIT-HASH INFO-STRING COLOR).")

(defvar-local git-blame-sidebar--commit-colors nil
  "Hash table mapping commit hash to color.")

(defvar-local git-blame-sidebar--last-sync-line 0
  "Last synchronized line number to avoid unnecessary updates.")

(defvar-local git-blame-sidebar--update-timer nil
  "Timer for delayed sidebar updates after buffer changes.")

(defvar-local git-blame-sidebar--source-buffer nil
  "The source file buffer for this sidebar.")

(defvar-local git-blame-sidebar--blame-process nil
  "Async process running git blame for this buffer (if any).")

(defvar-local git-blame-sidebar--commit-info-cache nil
  "Hash table commit -> info string (may be partial).")

(defvar-local git-blame-sidebar--commit-info-requests nil
  "Hash table commit -> process to avoid duplicate requests.")

(defvar-local git-blame-sidebar--last-failed-range nil
  "Store last failed range to avoid retrying the same failing request.")

(defvar-local git-blame-sidebar--pending-render-timer nil
  "Timer to debounce sidebar rendering after commit info arrives.")

(defvar-local git-blame-sidebar--needs-render nil
  "Flag indicating that the sidebar needs to be re-rendered.")

(defun git-blame-sidebar--ensure-commit-caches-in-buffer (buffer)
  "Ensure commit caches are initialized in BUFFER."
  (with-current-buffer buffer
	(unless git-blame-sidebar--commit-info-cache
	  (setq git-blame-sidebar--commit-info-cache (make-hash-table :test 'equal)))
	(unless git-blame-sidebar--commit-info-requests
	  (setq git-blame-sidebar--commit-info-requests (make-hash-table :test 'equal)))
	(unless git-blame-sidebar--commit-colors
	  (setq git-blame-sidebar--commit-colors (make-hash-table :test 'equal)))))

;;;; Utility Functions

(defun git-blame-sidebar--get-git-root ()
  "Get git root directory for current buffer."
  (when-let ((file (buffer-file-name)))
	(vc-git-root file)))

(defun git-blame-sidebar--generate-color (commit-hash)
  "Generate a consistent color for COMMIT-HASH."
  (let* ((hash-num (string-to-number (substring commit-hash 0 (min 8 (length commit-hash))) 16))
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
  (let ((buffer (current-buffer)))
	(git-blame-sidebar--ensure-commit-caches-in-buffer buffer)
	(or (gethash commit-hash git-blame-sidebar--commit-colors)
		(let ((color (git-blame-sidebar--generate-color commit-hash)))
		  (puthash commit-hash color git-blame-sidebar--commit-colors)
		  color))))

(defun git-blame-format-relative-time (author-time)
  "Format AUTHOR-TIME as a natural English relative time string like '3 minutes ago'.
For time spans >= 2 years, only the year part is shown (months are omitted)."
  (let* ((now (time-to-seconds (current-time)))
		 (diff (- now author-time)))
	(cond
	 ;; Future or invalid time → fall back to absolute date
	 ((< diff 0)
	  (format-time-string "%Y-%m-%d" (seconds-to-time author-time)))
	 ;; Less than 1 minute
	 ((< diff 60)
	  "just now")
	 ;; Minutes
	 ((< diff (* 60 60))
	  (format "%d minute%s ago"
			  (floor diff 60)
			  (if (= (floor diff 60) 1) "" "s")))
	 ;; Hours
	 ((< diff (* 60 60 24))
	  (format "%d hour%s ago"
			  (floor diff (* 60 60))
			  (if (= (floor diff (* 60 60)) 1) "" "s")))
	 ;; Days (< 30 days)
	 ((< diff (* 60 60 24 30))
	  (format "%d day%s ago"
			  (floor diff (* 60 60 24))
			  (if (= (floor diff (* 60 60 24)) 1) "" "s")))
	 ;; Months (< 1 year)
	 ((< diff (* 60 60 24 365))
	  (let ((months (git-blame--months-diff author-time now)))
		(format "%d month%s ago"
				months
				(if (= months 1) "" "s"))))
	 ;; >= 1 year
	 (t
	  (let ((years (git-blame--years-diff author-time now)))
		(if (>= years 2)
			;; ≥2 years: only show years
			(format "%d year%s ago" years (if (= years 1) "" "s"))
		  ;; 1 year <= span < 2 years: show year + months
		  (let ((extra-months (- (git-blame--months-diff author-time now) (* years 12))))
			(if (> extra-months 0)
				(format "%d year%s %d month%s ago"
						years (if (= years 1) "" "s")
						extra-months (if (= extra-months 1) "" "s"))
			  (format "%d year%s ago" years (if (= years 1) "" "s"))))))))))

(defun git-blame--months-diff (t1 t2)
  (let* ((d1 (decode-time (seconds-to-time t1)))
		 (d2 (decode-time (seconds-to-time t2))))
	(+ (* (- (nth 5 d2) (nth 5 d1)) 12)
	   (- (nth 4 d2) (nth 4 d1)))))

(defun git-blame--years-diff (t1 t2)
  (let* ((d1 (decode-time (seconds-to-time t1)))
		 (d2 (decode-time (seconds-to-time t2))))
	(let ((y1 (nth 5 d1)) (m1 (nth 4 d1)) (d1-day (nth 3 d1))
		  (y2 (nth 5 d2)) (m2 (nth 4 d2)) (d2-day (nth 3 d2)))
	  (let ((years (- y2 y1)))
		(if (or (< m2 m1)
				(and (= m2 m1) (< d2-day d1-day)))
			(1- years)
		  years)))))

;;;; Parsing Helpers (operate on strings)

(defun git-blame-sidebar--parse-blame-output (output source-buffer)
  "Parse --line-porcelain output to extract commit info directly."
  (with-temp-buffer
	(insert output)
	(goto-char (point-min))
	(let ((blame-data '())
		  (current-commit nil)
		  (current-line-num nil)
		  (author nil)
		  (author-time nil))
	  (while (not (eobp))
		(let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
		  (cond
		   ;; Commit header: HASH original-line final-line [num-lines]
		   ((string-match "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" line)
			(setq current-commit (match-string 1 line)
				  current-line-num (string-to-number (match-string 3 line)))
			(setq author nil author-time nil))

		   ;; Metadata lines
		   ((string-match "^author \\(.+\\)" line)
			(setq author (match-string 1 line)))
		   ((string-match "^author-time \\([0-9]+\\)" line)
			(setq author-time (string-to-number (match-string 1 line))))

		   ;; Code line (starts with tab)
		   ((string-prefix-p "\t" line)
			(when (and current-commit current-line-num)
			  (let* ((short-hash (substring current-commit 0 7))
					 (display-author (or author "Unknown"))
					 (display-time (if author-time
									   (git-blame-format-relative-time author-time)
									 "Unknown"))
					 (info-string (format "%s %s %s"
										  short-hash
										  display-author
										  display-time)))
				(push (list current-line-num current-commit info-string nil) blame-data)
				(setq current-line-num (1+ current-line-num)))))

		   ;; Ignore all other lines (committer, filename, etc.)
		   (t nil)))
		(forward-line 1))
	  (nreverse blame-data))))

;;;; Git Operations (robust versions)

(defun git-blame-sidebar--run-git (args &optional directory)
  "Run git command with ARGS in DIRECTORY, return (exit-code output error)."
  (let* ((default-directory (or directory default-directory))
		 (temp-out (make-temp-file "git-blame-out-"))
		 (temp-err (make-temp-file "git-blame-err-"))
		 (exit-code (call-process "git" nil (list temp-out temp-err) nil args))
		 (output (with-temp-buffer
				   (insert-file-contents temp-out)
				   (buffer-string)))
		 (error (with-temp-buffer
				  (insert-file-contents temp-err)
				  (buffer-string))))
	(delete-file temp-out)
	(delete-file temp-err)
	(list exit-code output error)))

;;;; Blame loader (with fallbacks)

(defun git-blame-sidebar--visible-line-range (buffer)
  "Return cons (START . END) lines to blame for BUFFER."
  (with-current-buffer buffer
	(let* ((total-lines (count-lines (point-min) (point-max)))
		   (win (get-buffer-window buffer))
		   (use-large-range (not git-blame-sidebar--blame-data)))

	  (if (not win)
		  (cons 1 (min (if use-large-range 1000 200) total-lines))

		(let* ((visible-start (line-number-at-pos (window-start win)))
			   (visible-end (line-number-at-pos (window-end win t)))
			   (radius (if use-large-range
						   (max 500 git-blame-sidebar-visible-radius)
						 git-blame-sidebar-visible-radius))
			   (s (max 1 (- visible-start radius)))
			   (e (min total-lines (+ visible-end radius))))
		  (cons s e))))))

(defun git-blame-sidebar--try-blame-range (source-buffer start end)
  "Try to run git blame on SOURCE-BUFFER for range START to END.
Return (exit-code output) or nil if git root not found."
  (let* ((git-root (git-blame-sidebar--get-git-root))
		 (file (buffer-file-name source-buffer))
		 (relative-file (when (and git-root file) (file-relative-name file git-root))))
	(when (and git-root file relative-file)
	  (let* ((default-directory git-root)
			 (cmd (append (list "blame" "--porcelain")
						  (when (and start end) (list (format "-L%d,%d" start end)))
						  (list relative-file)))
			 (result (apply #'git-blame-sidebar--run-git cmd)))
		result))))

(defun git-blame-sidebar--start-blame-process (source-buffer &optional start end)
  "Start an async `git blame --porcelain' for SOURCE-BUFFER with robust error handling."
  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer)

  (with-current-buffer source-buffer
	;; Skip if process is already running
	(when (process-live-p git-blame-sidebar--blame-process)
	  (message "[DEBUG] Process already running, skipping")
	  (cl-return-from git-blame-sidebar--start-blame-process nil))

	;; Skip if same range failed recently
	(when (and git-blame-sidebar--last-failed-range
			   (equal (list start end) git-blame-sidebar--last-failed-range))
	  (message "[DEBUG] SKIPPING: Same range failed recently")
	  (cl-return-from git-blame-sidebar--start-blame-process nil))

	(let* ((git-root (git-blame-sidebar--get-git-root))
		   (file (buffer-file-name source-buffer))
		   (relative-file (when (and git-root file) (file-relative-name file git-root))))

	  (unless (and git-root file relative-file)
		(message "[DEBUG] Missing git-root or file")
		(cl-return-from git-blame-sidebar--start-blame-process nil))

	  (let* ((default-directory git-root)
			 (cmd (append (list "git" "blame" "--line-porcelain")
						  (when (and start end) (list (format "-L%d,%d" start end)))
						  (list relative-file)))
			 (process-connection-type nil)
			 (proc nil))

		(message "[DEBUG] Starting blame: %s" (mapconcat 'identity cmd " "))

		(condition-case err
			(progn
			  (setq proc (apply #'start-process
								(format "git-blame-%s" (buffer-name source-buffer))
								(generate-new-buffer (format " *git-blame-%s*" (buffer-name source-buffer)))
								cmd))

			  (set-process-query-on-exit-flag proc nil)

			  (set-process-sentinel
			   proc
			   (lambda (p ev)
				 (message "[DEBUG] ===== SENTINEL CALLED ===== event='%s' status=%s"
						  (string-trim ev) (process-status p))

				 (when (memq (process-status p) '(exit signal))
				   (let* ((exit-status (process-exit-status p))
						  (out (with-current-buffer (process-buffer p) (buffer-string)))
						  (range (list start end))
						  (buffer (process-get p 'source-buffer)))

					 (message "[DEBUG] exit=%d out-size=%d" exit-status (length out))

					 (when (and buffer (buffer-live-p buffer))
					   (with-current-buffer buffer
						 (git-blame-sidebar--ensure-commit-caches-in-buffer buffer)

						 (if (and (= exit-status 0) (> (length out) 100))
							 (progn
							   (message "[DEBUG] Parsing output...")
							   (setq git-blame-sidebar--blame-data
									 (git-blame-sidebar--parse-blame-output out buffer))
							   (setq git-blame-sidebar--last-failed-range nil)
							   (message "[DEBUG] SUCCESS: Parsed %d lines"
										(length git-blame-sidebar--blame-data)))

						   (progn
							 (message "[DEBUG] FAILED: exit=%d" exit-status)
							 (setq git-blame-sidebar--blame-data nil)
							 (setq git-blame-sidebar--last-failed-range range)

							 (when (and start end)
							   (message "[DEBUG] Falling back to full file blame...")
							   (run-with-timer 0.5 nil
											   #'git-blame-sidebar--start-blame-process
											   buffer nil nil)))))

						 (message "[DEBUG] Cleaning up...")
						 (when (buffer-live-p (process-buffer p))
						   (kill-buffer (process-buffer p)))
						 (setq git-blame-sidebar--blame-process nil)

						 (message "[DEBUG] Updating sidebar...")
						 (git-blame-sidebar--update-sidebar buffer)
						 (message "[DEBUG] ===== DONE ====="))))))

			  (setq git-blame-sidebar--blame-process proc)
			  (process-put proc 'source-buffer source-buffer)

			  (message "[DEBUG] Process started: %s status=%s"
					   (process-name proc) (process-status proc))

			  (run-with-timer 15 nil
							  (lambda (p)
								(when (and (processp p)
										  (process-live-p p))
								  (message "[TIMEOUT] Process still running after 15s, killing...")
								  (delete-process p)))
							  proc)

			  nil)

		  (error
		   (message "[ERROR] Failed to create process: %s" err)
		   nil))))))

(defun git-blame-sidebar--fallback-sync-blame (source-buffer)
  "Fallback to synchronous git blame for SOURCE-BUFFER."
  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer)

  (with-current-buffer source-buffer
	(let* ((git-root (git-blame-sidebar--get-git-root))
		   (file (buffer-file-name))
		   (relative-file (when (and git-root file) (file-relative-name file git-root))))
	  (when (and git-root file relative-file)
		(let* ((default-directory git-root)
			   (result (git-blame-sidebar--try-blame-range source-buffer nil nil))
			   (exit-code (nth 0 result))
			   (output (nth 1 result))
			   (error (nth 2 result)))
		  (if (and (= exit-code 0) (not (string-empty-p output)))
			  (progn
				(setq git-blame-sidebar--blame-data (git-blame-sidebar--parse-blame-output output source-buffer))
				(message "Fallback to synchronous blame succeeded"))
			(progn
			  (setq git-blame-sidebar--blame-data nil)
			  (message "Synchronous blame also failed: %s" (string-trim error))
			  nil)))))))

(defun git-blame-sidebar--load-blame-data ()
  "Load git blame data asynchronously for current buffer."
  (let* ((file (buffer-file-name))
		 (git-root (git-blame-sidebar--get-git-root))
		 (buffer (current-buffer)))
	(when (and file git-root)
	  (let* ((range (git-blame-sidebar--visible-line-range buffer))
			 (start (car range)) (end (cdr range)))
		(when (> (line-number-at-pos (point-max)) git-blame-sidebar-max-lines)
		  (setq start (max 1 (- (line-number-at-pos (window-start)) 50))
				end (min (line-number-at-pos (point-max))
						 (+ (line-number-at-pos (window-end)) 50))))
		(git-blame-sidebar--start-blame-process buffer start end)
		git-blame-sidebar--blame-data))))

;;;; Async commit-info fetcher

(defun git-blame-sidebar--get-commit-info (commit-hash)
  "Get formatted info string for COMMIT-HASH synchronously."
  (with-temp-buffer
	(condition-case nil
		(if (zerop (call-process "git" nil t nil "show" "--no-patch"
								 (concat "--format=" git-blame-sidebar-format)
								 commit-hash))
			(string-trim (buffer-string))
		  (format "[Commit %s info unavailable]" (substring commit-hash 0 7)))
	  (error (format "[Error getting info for %s]" (substring commit-hash 0 7))))))

(defun git-blame-sidebar--request-commit-info-async (commit source-buffer)
  "Request formatted commit info for COMMIT asynchronously."
  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer)

  (with-current-buffer source-buffer
	(when (and (not (gethash commit git-blame-sidebar--commit-info-cache))
			   (not (gethash commit git-blame-sidebar--commit-info-requests)))
	  (let ((git-root (git-blame-sidebar--get-git-root)))
		(when git-root
		  (let* ((default-directory git-root)
				 (cmd (list "git" "show" "--no-patch"
							(concat "--format=" git-blame-sidebar-format)
							commit))
				 (process-connection-type nil)
				 (proc-buffer (generate-new-buffer (format " *git-show-%s*" (substring commit 0 8))))
				 (proc (apply #'start-process
							  (format "git-show-%s" (substring commit 0 8))
							  proc-buffer
							  cmd)))

			(set-process-query-on-exit-flag proc nil)
			(puthash commit proc git-blame-sidebar--commit-info-requests)
			(process-put proc 'source-buffer source-buffer)

			(set-process-sentinel
			 proc
			 (lambda (p ev)
			   (when (memq (process-status p) '(exit signal))
				 (let ((out (with-current-buffer (process-buffer p) (buffer-string)))
					   (exit (process-exit-status p))
					   (buffer (process-get p 'source-buffer)))

				   (when (and buffer (buffer-live-p buffer))
					 (with-current-buffer buffer
					   (git-blame-sidebar--ensure-commit-caches-in-buffer buffer)
					   (puthash commit
								(if (and (= exit 0) (not (string-empty-p out)))
									(string-trim out)
								  (format "[Error %s]" (substring commit 0 8)))
								git-blame-sidebar--commit-info-cache)
					   (remhash commit git-blame-sidebar--commit-info-requests)

					   (setq git-blame-sidebar--needs-render t)

					   (unless git-blame-sidebar--pending-render-timer
						 (setq git-blame-sidebar--pending-render-timer
							   (run-with-idle-timer
								0.01 nil
								(lambda ()
								  (when (and git-blame-sidebar--needs-render
											 (buffer-live-p git-blame-sidebar--global-sidebar-buffer)
											 (buffer-live-p buffer))
									(with-current-buffer buffer
									  (setq git-blame-sidebar--needs-render nil
											git-blame-sidebar--pending-render-timer nil)
									  (git-blame-sidebar--update-sidebar buffer))))))))
				 (when (buffer-live-p (process-buffer p))
				   (kill-buffer (process-buffer p))))))))))))))

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
  "Render BLAME-DATA in SIDEBAR-BUFFER without destroying text properties."
  (let ((source-buffer git-blame-sidebar--global-source-buffer))
	(when (and source-buffer (buffer-live-p source-buffer))
	  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer))

	(with-current-buffer sidebar-buffer
	  (let ((inhibit-read-only t)
			(buffer-undo-list t))
		(erase-buffer)
		(setq truncate-lines nil)

		(cond
		 ((not (and source-buffer (buffer-live-p source-buffer)))
		  (insert (propertize "❌ No source buffer available" 'face '(:foreground "red"))))

		 ((null blame-data)
		  (let ((total-lines (with-current-buffer source-buffer
							   (save-restriction (widen)
								 (count-lines (point-min) (point-max))))))
			(when (> total-lines 0)
			  (insert (propertize "⏳ Loading blame data...\n"
								  'face '(:weight bold :height 1.2 :background "yellow1" :foreground "black"))))))

		 (t
		  (let* ((source-total-lines
				  (with-current-buffer source-buffer
					(save-restriction (widen)
					  (count-lines (point-min) (point-max)))))
				 (blame-alist (cl-loop for entry in blame-data
									   collect (cons (nth 0 entry) entry)))
				 (line-num 1))

			(while (<= line-num source-total-lines)
			  (let ((entry (cdr (assoc line-num blame-alist))))
				(if entry
					(let* ((commit (nth 1 entry))
						   (raw-info (or (nth 2 entry) (format "%s..." (substring commit 0 7))))
						   (clean-info (string-trim
										(replace-regexp-in-string "  +" " "
														  (replace-regexp-in-string "[\15\n\11]+" " " (or raw-info "")))))
						   (color (or (nth 3 entry)
									  (with-current-buffer source-buffer
										(git-blame-sidebar--get-commit-color commit))
									  "#888888")))
					  (let ((start (point)))
						(insert (propertize "▌" 'face `(:foreground ,color)) " ")
						(insert (propertize clean-info 'face `(:foreground ,color)))
						(add-text-properties start (point)
											 `(commit-hash ,commit line-number ,line-num)))
					  (insert "\n"))
				  (let ((start (point)))
					(insert " ")
					(add-text-properties start (1+ start) `(line-number ,line-num))
					(insert "\n"))))
			  (setq line-num (1+ line-num))))))

		(goto-char (point-min))))))

(defun git-blame-sidebar--update-sidebar (source-buffer)
  "Update sidebar for SOURCE-BUFFER with safety checks."
  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer)

  (when-let* ((sidebar-buffer git-blame-sidebar--global-sidebar-buffer)
			  (buffer-live-p sidebar-buffer))
	(with-current-buffer source-buffer
	  (if git-blame-sidebar--blame-data
		  (git-blame-sidebar--render-sidebar sidebar-buffer git-blame-sidebar--blame-data)
		(git-blame-sidebar--render-sidebar sidebar-buffer nil)))))

;;;; Interaction and hooks

(defun git-blame-sidebar--source-post-command ()
  "Hook function for source buffer post-command with safety checks."
  (when-let* ((sidebar-buffer git-blame-sidebar--global-sidebar-buffer)
			  (buffer-live-p sidebar-buffer)
			  (sidebar-window (get-buffer-window sidebar-buffer))
			  (window-live-p sidebar-window)
			  (current-line (line-number-at-pos))
			  (source-buffer (current-buffer)))
	(when (/= current-line git-blame-sidebar--last-sync-line)
	  (setq git-blame-sidebar--last-sync-line current-line)
	  (let* ((sidebar-line-count (with-current-buffer sidebar-buffer
								   (count-lines (point-min) (point-max))))
			 (target-line (min current-line sidebar-line-count)))
		(with-current-buffer sidebar-buffer
		  (condition-case nil
			  (progn
				(goto-char (point-min))
				(forward-line (max 0 (1- target-line)))
				(set-window-point sidebar-window (point)))
			(args-out-of-range
			 (message "Warning: Could not sync to line %d" target-line))))))))

(defun git-blame-sidebar--after-change-function (beg end _)
  "Do nothing on buffer changes to keep blame stable."
  ;; Intentionally left blank — do NOT reload blame on edit.
  )

;;;; Auto-follow logic (NEW)

(defun git-blame-sidebar--on-window-selection-change (&optional _frame)
  "Called when user selects a different window."
  (when git-blame-sidebar--global-sidebar-buffer
	(when git-blame-sidebar--global-follow-timer
	  (cancel-timer git-blame-sidebar--global-follow-timer))
	(setq git-blame-sidebar--global-follow-timer
		  (run-with-idle-timer
		   0.05 nil
		   #'git-blame-sidebar--maybe-switch-to-current-buffer))))

(defun git-blame-sidebar--on-window-config-change ()
  "Fallback hook for cases like `quit-window` that don't trigger selection change."
  (when git-blame-sidebar--global-sidebar-buffer
	;; Use the same debounce timer as selection change
	(when git-blame-sidebar--global-follow-timer
	  (cancel-timer git-blame-sidebar--global-follow-timer))
	(setq git-blame-sidebar--global-follow-timer
		  (run-with-idle-timer
		   0.05 nil
		   #'git-blame-sidebar--maybe-switch-to-current-buffer))))

(defun git-blame-sidebar--maybe-switch-to-current-buffer ()
  "Check if current buffer is a valid Git file and different from sidebar's source."
  (let* ((current-buf (current-buffer))
		 (old-source git-blame-sidebar--global-source-buffer))
	(when (and (buffer-file-name current-buf)
			   (git-blame-sidebar--get-git-root)
			   (not (string-prefix-p "*" (buffer-name current-buf)))
			   (not (eq current-buf old-source))
			   (not (string-prefix-p "*git-blame:" (buffer-name current-buf))))
	  (git-blame-sidebar--switch-to-buffer current-buf))))

(defun git-blame-sidebar--switch-to-buffer (new-buffer)
  "Switch sidebar to NEW-BUFFER."
  ;; Clean up old source buffer hooks
  (when (and git-blame-sidebar--global-source-buffer
			 (buffer-live-p git-blame-sidebar--global-source-buffer))
	(with-current-buffer git-blame-sidebar--global-source-buffer
	  (remove-hook 'post-command-hook #'git-blame-sidebar--source-post-command t)))

  ;; Set new source
  (setq git-blame-sidebar--global-source-buffer new-buffer)

  (with-current-buffer new-buffer
	(git-blame-sidebar--ensure-commit-caches-in-buffer new-buffer)
	(setq git-blame-sidebar--blame-data nil
		  git-blame-sidebar--last-sync-line 0
		  git-blame-sidebar--buffer git-blame-sidebar--global-sidebar-buffer)
	(add-hook 'post-command-hook #'git-blame-sidebar--source-post-command nil t))

  ;; Render loading
  (git-blame-sidebar--render-sidebar git-blame-sidebar--global-sidebar-buffer nil)

  ;; Start blame
  (run-with-idle-timer
   0.01 nil
   (lambda (buf)
	 (when (buffer-live-p buf)
	   (with-current-buffer buf
		 (let ((total-lines (count-lines (point-min) (point-max))))
		   (if (> total-lines git-blame-sidebar-max-lines)
			   (git-blame-sidebar--start-blame-process buf 1 git-blame-sidebar-max-lines)
			 (git-blame-sidebar--start-blame-process buf nil nil))))))
   new-buffer))

;;;; Window Management

(defun git-blame-sidebar--show-sidebar (source-buffer)
  "Show sidebar for SOURCE-BUFFER with safety checks."
  (git-blame-sidebar--ensure-commit-caches-in-buffer source-buffer)

  (let ((sidebar-buffer (or git-blame-sidebar--global-sidebar-buffer
							(git-blame-sidebar--create-sidebar-buffer source-buffer))))
	(setq git-blame-sidebar--global-sidebar-buffer sidebar-buffer
		  git-blame-sidebar--global-source-buffer source-buffer)

	(with-current-buffer source-buffer
	  (setq git-blame-sidebar--buffer sidebar-buffer
			git-blame-sidebar--last-sync-line 0)
	  (add-hook 'post-command-hook #'git-blame-sidebar--source-post-command nil t))

	;; Start blame process for the whole file
	(with-current-buffer source-buffer
	  (setq git-blame-sidebar--blame-data nil)
	  (if git-blame-sidebar-async
		  (git-blame-sidebar--start-blame-process source-buffer nil nil)
		(git-blame-sidebar--fallback-sync-blame source-buffer)))

	;; Register auto-follow hook
	(add-hook 'window-selection-change-functions #'git-blame-sidebar--on-window-selection-change)
	;; Fallback for quit-window, winner-undo, etc.
	(add-hook 'window-configuration-change-hook #'git-blame-sidebar--on-window-config-change)

	;; Display sidebar window
	(let ((window (display-buffer-in-side-window
				   sidebar-buffer
				   `((side . ,git-blame-sidebar-position)
					 (window-width . ,git-blame-sidebar-width)
					 (slot . 0)
					 (window-parameters . ((no-other-window . t)
										  (dedicated . t)))))))
	  (set-window-dedicated-p window t)

	  (git-blame-sidebar--render-sidebar sidebar-buffer nil)

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
		  git-blame-sidebar--global-source-buffer nil
		  git-blame-sidebar--global-follow-timer nil)
	(remove-hook 'window-selection-change-functions #'git-blame-sidebar--on-window-selection-change)
	(remove-hook 'window-configuration-change-hook #'git-blame-sidebar--on-window-config-change)
	(with-current-buffer source-buffer
	  (setq git-blame-sidebar--buffer nil
			git-blame-sidebar--last-sync-line 0
			git-blame-sidebar--blame-data nil
			git-blame-sidebar--commit-colors nil
			git-blame-sidebar--commit-info-cache nil
			git-blame-sidebar--commit-info-requests nil)
	  (remove-hook 'post-command-hook #'git-blame-sidebar--source-post-command t)
	  (remove-hook 'after-change-functions #'git-blame-sidebar--after-change-function t)
	  (when git-blame-sidebar--update-timer
		(cancel-timer git-blame-sidebar--update-timer)
		(setq git-blame-sidebar--update-timer nil)))))

;;;; Commands (copy/diff reuse commit-info-cache)

(defun git-blame-sidebar-copy-commit-hash ()
  "Copy commit hash for current line.
Works in source buffer (via blame cache) or sidebar buffer (via text property)."
  (interactive)
  (let (commit)
	(cond
	 ((and (boundp 'git-blame-sidebar-mode)
		   (derived-mode-p 'git-blame-sidebar-mode))
	  (setq commit (get-text-property (point) 'commit-hash)))

	 ((and (buffer-local-value 'git-blame-sidebar--blame-data (current-buffer))
		   (not (string-prefix-p "*" (buffer-name))))
	  (let ((line-num (line-number-at-pos))
			(blame-data (buffer-local-value 'git-blame-sidebar--blame-data (current-buffer))))
		(when blame-data
		  (let ((entry (cl-find-if (lambda (e) (= (car e) line-num)) blame-data)))
			(setq commit (nth 1 entry))))))

	 ((and git-blame-sidebar--global-source-buffer
		   (buffer-live-p git-blame-sidebar--global-source-buffer))
	  (let* ((source-buf git-blame-sidebar--global-source-buffer)
			 (blame-data (buffer-local-value 'git-blame-sidebar--blame-data source-buf)))
		(when blame-data
		  (let ((line-num (with-current-buffer source-buf
							(line-number-at-pos (window-point)))))
			(let ((entry (cl-find-if (lambda (e) (= (car e) line-num)) blame-data)))
			  (setq commit (nth 1 entry))))))))

	(if commit
		(progn
		  (kill-new commit)
		  (message "Copied commit hash: %s" (substring commit 0 8)))
	  (message "No blame data available for current line"))))

(defun git-blame-sidebar-show-commit-diff ()
  "Show diff of commit for current line and switch to it.
Works in source buffer (via blame cache) or sidebar buffer (via text property)."
  (interactive)
  (let (commit)
	(cond
	 ((and (boundp 'git-blame-sidebar-mode)
		   (derived-mode-p 'git-blame-sidebar-mode))
	  (setq commit (get-text-property (point) 'commit-hash)))

	 ((and (buffer-local-value 'git-blame-sidebar--blame-data (current-buffer))
		   (not (string-prefix-p "*" (buffer-name))))
	  (let ((line-num (line-number-at-pos))
			(blame-data (buffer-local-value 'git-blame-sidebar--blame-data (current-buffer))))
		(when blame-data
		  (let ((entry (cl-find-if (lambda (e) (= (car e) line-num)) blame-data)))
			(setq commit (nth 1 entry))))))

	 ((and git-blame-sidebar--global-source-buffer
		   (buffer-live-p git-blame-sidebar--global-source-buffer))
	  (let* ((source-buf git-blame-sidebar--global-source-buffer)
			 (blame-data (buffer-local-value 'git-blame-sidebar--blame-data source-buf)))
		(when blame-data
		  (let ((line-num (with-current-buffer source-buf
							(line-number-at-pos (window-point)))))
			(let ((entry (cl-find-if (lambda (e) (= (car e) line-num)) blame-data)))
			  (setq commit (nth 1 entry))))))))

	(if (and commit (git-blame-sidebar--get-git-root))
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
				(kill-buffer)
				(message "Failed to show diff for commit %s" commit)))))
	  (message "No blame data available for current line"))))

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

(defun git-blame-sidebar-debug-current-state ()
  "Debug current blame sidebar state."
  (interactive)
  (let* ((source-buf (or git-blame-sidebar--global-source-buffer (current-buffer)))
		 (sidebar-buf git-blame-sidebar--global-sidebar-buffer)
		 (process (buffer-local-value 'git-blame-sidebar--blame-process source-buf)
				  )
		 (data (buffer-local-value 'git-blame-sidebar--blame-data source-buf))
		 (cache (buffer-local-value 'git-blame-sidebar--commit-info-cache source-buf)))
	(message "=== GIT BLAME SIDEBAR DEBUG ===")
	(message "Source buffer: %s" (buffer-name source-buf))
	(message "Sidebar buffer: %s" (if sidebar-buf (buffer-name sidebar-buf) "nil"))
	(message "Git root: %s" (git-blame-sidebar--get-git-root))
	(message "Blame process: %s" (if process (process-status process) "none"))
	(message "Blame data entries: %s" (if data (length data) "nil"))
	(message "Commit cache size: %s" (if (and cache (hash-table-p cache))
										  (hash-table-count cache) "invalid"))
	(when process
	  (message "Process command: %s" (process-command process))
	  (message "Process buffer: %s" (buffer-name (process-buffer process))))
	(message "=============================")))

;;;###autoload
(defun git-blame-sidebar-refresh ()
  "Manually refresh git blame data for current buffer."
  (interactive)
  (if (not git-blame-sidebar--global-sidebar-buffer)
	  (message "Git blame sidebar is not active")
	(let ((source-buf git-blame-sidebar--global-source-buffer))
	  (when (buffer-live-p source-buf)
		(with-current-buffer source-buf
		  (setq git-blame-sidebar--blame-data nil)
		  (if git-blame-sidebar-async
			  (git-blame-sidebar--start-blame-process source-buf nil nil)
			(git-blame-sidebar--fallback-sync-blame source-buf))
		  (message "Refreshing git blame data..."))))))

(define-derived-mode git-blame-sidebar-mode special-mode "GitBlame"
  "Major mode for git blame sidebar."
  :group 'git-blame-sidebar
  (setq truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 10000)
  (setq-local cursor-type nil)
  (setq-local mode-line-format nil))

(provide 'git-blame-sidebar)
;;; git-blame-sidebar.el ends here
