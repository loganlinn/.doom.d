;;; autoload/+kitty-rc.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Kitty remote control via `kitty @` CLI.
;; Provides interactive Emacs commands for all kitty remote control operations.

;;; Code:

(require 'json)

;;; Customization

;;;###autoload
(defgroup kitty-rc nil
  "Kitty terminal remote control."
  :group 'terminals
  :prefix "kitty-rc-")

;;;###autoload
(defcustom kitty-rc-executable "kitty"
  "Path to kitty executable."
  :type 'string
  :group 'kitty-rc)

;;;###autoload
(defcustom kitty-rc-socket nil
  "Socket path for kitty remote control.
When nil, uses KITTY_LISTEN_ON or TTY-based control."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Socket path"))
  :group 'kitty-rc)

;;; Core

(defun kitty-rc--build-args (cmd &rest args)
  "Build argument list for kitty @ CMD with ARGS."
  (let ((base (list "@")))
    (when kitty-rc-socket
      (setq base (append base (list "--to" kitty-rc-socket))))
    (append base (list cmd) (flatten-list args))))

;;;###autoload
(defun kitty-rc-call (cmd &rest args)
  "Call kitty @ CMD with ARGS synchronously.
Returns output as string, or nil on error."
  (let ((args (apply #'kitty-rc--build-args cmd args)))
    (with-temp-buffer
      (if (zerop (apply #'call-process kitty-rc-executable nil t nil args))
          (buffer-string)
        (message "kitty @ %s failed: %s" cmd (buffer-string))
        nil))))

;;;###autoload
(defun kitty-rc-call-json (cmd &rest args)
  "Call kitty @ CMD with ARGS, parse JSON response."
  (when-let ((output (apply #'kitty-rc-call cmd args)))
    (unless (string-empty-p output)
      (json-parse-string output :object-type 'alist :array-type 'list))))

;;;###autoload
(defun kitty-rc-call-async (cmd callback &rest args)
  "Call kitty @ CMD with ARGS asynchronously.
CALLBACK receives (success output) args."
  (let* ((args (apply #'kitty-rc--build-args cmd args))
         (buf (generate-new-buffer " *kitty-rc*")))
    (make-process
     :name "kitty-rc"
     :buffer buf
     :command (cons kitty-rc-executable args)
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer buf
                     (funcall callback
                              (zerop (process-exit-status proc))
                              (buffer-string)))
                   (kill-buffer buf))))))

;;; Match helpers

(defun kitty-rc--match (type value)
  "Build match string for TYPE and VALUE."
  (format "%s:%s" type value))

(defun kitty-rc--match-id (id)
  "Match by window ID."
  (kitty-rc--match "id" id))

(defun kitty-rc--match-title (title)
  "Match by window title (regex)."
  (kitty-rc--match "title" title))

(defun kitty-rc--match-recent (n)
  "Match Nth recent window (0=active)."
  (kitty-rc--match "recent" n))

;;; Window/Tab listing

;;;###autoload
(defun kitty-rc-ls ()
  "List all kitty OS windows, tabs, and windows."
  (kitty-rc-call-json "ls"))

(defun kitty-rc--collect-windows ()
  "Collect all windows as alist of (display . id)."
  (let ((windows nil))
    (dolist (os-win (kitty-rc-ls))
      (dolist (tab (alist-get 'tabs os-win))
        (let ((tab-title (alist-get 'title tab)))
          (dolist (win (alist-get 'windows tab))
            (let* ((win-id (alist-get 'id win))
                   (win-title (alist-get 'title win))
                   (fg (alist-get 'foreground_processes win))
                   (cmdline (when fg (alist-get 'cmdline (car fg))))
                   (cmd (if cmdline (string-join cmdline " ") ""))
                   (display (format "%d: %s [%s] (tab: %s)"
                                    win-id win-title cmd tab-title)))
              (push (cons display win-id) windows))))))
    (nreverse windows)))

(defun kitty-rc--collect-tabs ()
  "Collect all tabs as alist of (display . id)."
  (let ((tabs nil))
    (dolist (os-win (kitty-rc-ls))
      (dolist (tab (alist-get 'tabs os-win))
        (let* ((tab-id (alist-get 'id tab))
               (tab-title (alist-get 'title tab))
               (active (alist-get 'is_active tab))
               (display (format "%d: %s%s"
                                tab-id tab-title
                                (if active " (active)" ""))))
          (push (cons display tab-id) tabs))))
    (nreverse tabs)))

(defun kitty-rc--read-window (prompt)
  "Read window selection with PROMPT."
  (let* ((windows (kitty-rc--collect-windows))
         (choice (completing-read prompt windows nil t)))
    (alist-get choice windows nil nil #'equal)))

(defun kitty-rc--active-tab-id ()
  "Get the ID of the currently active tab."
  (catch 'found
    (dolist (os-win (kitty-rc-ls))
      (dolist (tab (alist-get 'tabs os-win))
        (when (eq (alist-get 'is_active tab) t)
          (throw 'found (alist-get 'id tab)))))))

(defun kitty-rc--window-is-claude-p (win)
  "Return non-nil if WIN is running Claude Code."
  (let* ((fg (alist-get 'foreground_processes win))
         (cmdline (when fg (alist-get 'cmdline (car fg)))))
    (when cmdline
      (seq-some (lambda (arg) (string-match-p "\\bclaude\\b" arg))
                cmdline))))

(defun kitty-rc--window-mru-score (win-id tab os-win)
  "Calculate MRU score for WIN-ID (higher = more recent).
Considers position in tab's window history, tab's position in OS window history,
and whether OS window is focused."
  (let* ((os-focused (eq (alist-get 'is_focused os-win) t))
         (tab-active (eq (alist-get 'is_active tab) t))
         (tab-id (alist-get 'id tab))
         (tab-history (alist-get 'active_tab_history os-win))
         (win-history (alist-get 'active_window_history tab))
         (tab-pos (or (cl-position tab-id tab-history) 999))
         (win-pos (or (cl-position win-id win-history) 999)))
    ;; Score: focused OS wins get +10000, active tab +1000, then inverse positions
    (+ (if os-focused 10000 0)
       (if tab-active 1000 0)
       (- 100 (min tab-pos 100))
       (- 10 (min win-pos 10)))))

(defun kitty-rc--collect-claude-windows (&optional current-tab-only)
  "Collect Claude Code windows as alist of (display . id), ordered by MRU.
If CURRENT-TAB-ONLY is non-nil, only search active tab."
  (let ((windows nil)
        (active-tab-id (when current-tab-only (kitty-rc--active-tab-id))))
    (dolist (os-win (kitty-rc-ls))
      (dolist (tab (alist-get 'tabs os-win))
        (when (or (not current-tab-only)
                  (eq (alist-get 'id tab) active-tab-id))
          (let ((tab-title (alist-get 'title tab)))
            (dolist (win (alist-get 'windows tab))
              (when (kitty-rc--window-is-claude-p win)
                (let* ((win-id (alist-get 'id win))
                       (win-title (alist-get 'title win))
                       (fg (alist-get 'foreground_processes win))
                       (cmdline (when fg (alist-get 'cmdline (car fg))))
                       (cmd (if cmdline (string-join cmdline " ") ""))
                       (score (kitty-rc--window-mru-score win-id tab os-win))
                       (display (format "%d: %s [%s] (tab: %s)"
                                        win-id win-title cmd tab-title)))
                  (push (list display win-id score) windows))))))))
    ;; Sort by score descending, return as (display . id) alist
    (mapcar (lambda (entry) (cons (car entry) (cadr entry)))
            (sort windows (lambda (a b) (> (caddr a) (caddr b)))))))

(defun kitty-rc--read-claude-window (prompt &optional current-tab-only)
  "Select a Claude window with PROMPT.
If CURRENT-TAB-ONLY is non-nil, only show windows in active tab."
  (let* ((windows (kitty-rc--collect-claude-windows current-tab-only))
         (choice (completing-read prompt windows nil t)))
    (alist-get choice windows nil nil #'equal)))

(defvar kitty-rc--last-claude-window nil
  "Window ID of last used Claude Code window.")

(defun kitty-rc--read-tab (prompt)
  "Read tab selection with PROMPT."
  (let* ((tabs (kitty-rc--collect-tabs))
         (choice (completing-read prompt tabs nil t)))
    (alist-get choice tabs nil nil #'equal)))

;;; Window commands

;;;###autoload
(defun kitty-rc-focus-window (match)
  "Focus window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Focus window: "))))
  (kitty-rc-call "focus-window" "--match" match))

;;;###autoload
(defun kitty-rc-close-window (match)
  "Close window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Close window: "))))
  (kitty-rc-call "close-window" "--match" match))

;;;###autoload
(defun kitty-rc-new-window (&optional args)
  "Create new kitty window.
With prefix arg, prompts for additional arguments."
  (interactive (list (when current-prefix-arg
                       (split-string (read-string "Args: ")))))
  (apply #'kitty-rc-call "new-window" args))

;;;###autoload
(defun kitty-rc-new-os-window ()
  "Create new OS window."
  (interactive)
  (kitty-rc-call "launch" "--type=os-window"))

;;;###autoload
(defun kitty-rc-resize-window (match increment axis)
  "Resize window matching MATCH by INCREMENT cells on AXIS."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Resize window: "))
         (read-number "Increment: " 1)
         (completing-read "Axis: " '("horizontal" "vertical" "reset") nil t)))
  (kitty-rc-call "resize-window" "--match" match
                 "--increment" (number-to-string increment)
                 "--axis" axis))

;;;###autoload
(defun kitty-rc-set-window-title (match title)
  "Set TITLE for window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Window: "))
         (read-string "Title: ")))
  (kitty-rc-call "set-window-title" "--match" match title))

;;;###autoload
(defun kitty-rc-detach-window (match target-tab)
  "Detach window matching MATCH to TARGET-TAB (or new tab if 'new')."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Detach window: "))
         (let ((choice (completing-read "Target: " '("new" "select tab"))))
           (if (equal choice "new") "new"
             (format "id:%d" (kitty-rc--read-tab "Target tab: "))))))
  (kitty-rc-call "detach-window" "--match" match "--target-tab" target-tab))

;;; Tab commands

;;;###autoload
(defun kitty-rc-focus-tab (match)
  "Focus tab matching MATCH."
  (interactive
   (list (kitty-rc--match "id" (kitty-rc--read-tab "Focus tab: "))))
  (kitty-rc-call "focus-tab" "--match" match))

;;;###autoload
(defun kitty-rc-close-tab (match)
  "Close tab matching MATCH."
  (interactive
   (list (kitty-rc--match "id" (kitty-rc--read-tab "Close tab: "))))
  (kitty-rc-call "close-tab" "--match" match))

;;;###autoload
(defun kitty-rc-new-tab (&optional args)
  "Create new tab."
  (interactive (list (when current-prefix-arg
                       (split-string (read-string "Args: ")))))
  (apply #'kitty-rc-call "launch" "--type=tab" args))

;;;###autoload
(defun kitty-rc-set-tab-title (match title)
  "Set TITLE for tab matching MATCH."
  (interactive
   (list (kitty-rc--match "id" (kitty-rc--read-tab "Tab: "))
         (read-string "Title: ")))
  (kitty-rc-call "set-tab-title" "--match" match title))

;;;###autoload
(defun kitty-rc-detach-tab (match)
  "Detach tab matching MATCH to new OS window."
  (interactive
   (list (kitty-rc--match "id" (kitty-rc--read-tab "Detach tab: "))))
  (kitty-rc-call "detach-tab" "--match" match))

;;; Layout commands

;;;###autoload
(defun kitty-rc-goto-layout (layout)
  "Switch to LAYOUT."
  (interactive
   (list (completing-read "Layout: "
                          '("fat" "grid" "horizontal" "splits"
                            "stack" "tall" "vertical"))))
  (kitty-rc-call "goto-layout" layout))

;;;###autoload
(defun kitty-rc-last-used-layout ()
  "Switch to last used layout."
  (interactive)
  (kitty-rc-call "last-used-layout"))

;;;###autoload
(defun kitty-rc-set-enabled-layouts (layouts)
  "Set enabled LAYOUTS (space-separated)."
  (interactive "sLayouts: ")
  (kitty-rc-call "set-enabled-layouts" layouts))

;;; Text/Content commands

;;;###autoload
(defun kitty-rc-send-text (match text)
  "Send TEXT to window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Send to window: "))
         (read-string "Text: ")))
  (kitty-rc-call "send-text" "--match" match text))

;;;###autoload
(defun kitty-rc-send-key (match keys)
  "Send KEYS to window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Send to window: "))
         (read-string "Keys: ")))
  (kitty-rc-call "send-key" "--match" match keys))

;;;###autoload
(defun kitty-rc-get-text (match &optional extent)
  "Get text from window matching MATCH.
EXTENT is one of: screen, all, selection, first_cmd_output_on_screen,
last_cmd_output, last_visited_cmd_output, last_non_empty_output."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Get text from: "))
         (completing-read "Extent: "
                          '("screen" "all" "selection" "first_cmd_output_on_screen"
                            "last_cmd_output" "last_visited_cmd_output"
                            "last_non_empty_output")
                          nil t nil nil "screen")))
  (let ((text (kitty-rc-call "get-text" "--match" match "--extent" (or extent "screen"))))
    (when (called-interactively-p 'any)
      (kill-new text)
      (message "Copied %d chars" (length text)))
    text))

;;;###autoload
(defun kitty-rc-scroll-window (match amount)
  "Scroll window matching MATCH by AMOUNT.
AMOUNT can be integer lines, or: start, end, page-up, page-down."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Scroll window: "))
         (read-string "Amount (lines or start/end/page-up/page-down): " "page-down")))
  (kitty-rc-call "scroll-window" "--match" match amount))

;;; Appearance commands

;;;###autoload
(defun kitty-rc-set-font-size (size)
  "Set font SIZE. Prefix with +/- for relative."
  (interactive "sFont size: ")
  (kitty-rc-call "set-font-size" size))

;;;###autoload
(defun kitty-rc-set-background-opacity (opacity)
  "Set background OPACITY (0.0-1.0, or +/- for relative)."
  (interactive "sOpacity: ")
  (kitty-rc-call "set-background-opacity" opacity))

;;;###autoload
(defun kitty-rc-set-colors (&rest colors)
  "Set terminal COLORS as key=value pairs."
  (interactive (split-string (read-string "Colors (key=value ...): ")))
  (apply #'kitty-rc-call "set-colors" colors))

;;;###autoload
(defun kitty-rc-get-colors ()
  "Get current terminal colors."
  (interactive)
  (let ((output (kitty-rc-call "get-colors")))
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create "*kitty-colors*")
        (erase-buffer)
        (insert output)
        (pop-to-buffer (current-buffer))))
    output))

;;;###autoload
(defun kitty-rc-set-background-image (path)
  "Set background image to PATH."
  (interactive "fImage: ")
  (kitty-rc-call "set-background-image" (expand-file-name path)))

;;; Launch commands

;;;###autoload
(defun kitty-rc-launch (cmd &optional type cwd)
  "Launch CMD in kitty.
TYPE is window type: window, tab, os-window, overlay, background.
CWD is working directory."
  (interactive
   (list (read-shell-command "Command: ")
         (completing-read "Type: "
                          '("window" "tab" "os-window" "overlay" "background")
                          nil t nil nil "window")
         (read-directory-name "Working directory: " nil nil nil)))
  (let ((args (list "--type" (or type "window"))))
    (when (and cwd (not (string-empty-p cwd)))
      (setq args (append args (list "--cwd" (expand-file-name cwd)))))
    (apply #'kitty-rc-call "launch" (append args (split-string-shell-command cmd)))))

;;;###autoload
(defun kitty-rc-run (cmd)
  "Run CMD on kitty host and return output."
  (interactive "sCommand: ")
  (let ((output (apply #'kitty-rc-call "run" (split-string-shell-command cmd))))
    (when (called-interactively-p 'any)
      (message "%s" output))
    output))

;;; Kitten commands

;;;###autoload
(defun kitty-rc-kitten (kitten &rest args)
  "Run KITTEN with ARGS."
  (interactive (list (read-string "Kitten: ")
                     (split-string (read-string "Args: "))))
  (apply #'kitty-rc-call "kitten" kitten (flatten-list args)))

;;;###autoload
(defun kitty-rc-broadcast-on ()
  "Enable broadcast mode (type in all windows)."
  (interactive)
  (kitty-rc-call "action" "toggle_broadcast" "on"))

;;;###autoload
(defun kitty-rc-broadcast-off ()
  "Disable broadcast mode."
  (interactive)
  (kitty-rc-call "action" "toggle_broadcast" "off"))

;;; Action command

;;;###autoload
(defun kitty-rc-action (action &rest args)
  "Run mappable ACTION with ARGS."
  (interactive "sAction: ")
  (apply #'kitty-rc-call "action" action args))

;;; Marker commands

;;;###autoload
(defun kitty-rc-create-marker (match type spec)
  "Create marker in window matching MATCH.
TYPE is marker type: text, regex, itext, function.
SPEC is the marker specification."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Window: "))
         (completing-read "Type: " '("text" "regex" "itext" "function"))
         (read-string "Spec (number color spec): ")))
  (kitty-rc-call "create-marker" "--match" match
                 (format "%s:%s" type spec)))

;;;###autoload
(defun kitty-rc-remove-marker (match)
  "Remove marker from window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Window: "))))
  (kitty-rc-call "remove-marker" "--match" match))

;;; OS Window commands

;;;###autoload
(defun kitty-rc-resize-os-window (action &optional unit amount)
  "Resize OS window.
ACTION is: toggle-fullscreen, toggle-maximized, width, height."
  (interactive
   (let ((action (completing-read "Action: "
                                  '("toggle-fullscreen" "toggle-maximized"
                                    "width" "height"))))
     (list action
           (when (member action '("width" "height"))
             (completing-read "Unit: " '("cells" "pixels")))
           (when (member action '("width" "height"))
             (read-number "Amount: ")))))
  (let ((args (list "--action" action)))
    (when unit (setq args (append args (list "--unit" unit))))
    (when amount (setq args (append args (list "--increment" (number-to-string amount)))))
    (apply #'kitty-rc-call "resize-os-window" args)))

;;; Config commands

;;;###autoload
(defun kitty-rc-load-config (&optional paths)
  "Reload kitty config, optionally from specific PATHS."
  (interactive)
  (apply #'kitty-rc-call "load-config" paths))

;;;###autoload
(defun kitty-rc-env (name &optional value)
  "Set environment variable NAME to VALUE for future children.
With no VALUE, unsets the variable."
  (interactive "sVariable: \nsValue: ")
  (if (or (null value) (string-empty-p value))
      (kitty-rc-call "env" (format "%s=" name))
    (kitty-rc-call "env" (format "%s=%s" name value))))

;;; Signal commands

;;;###autoload
(defun kitty-rc-signal-child (match signal)
  "Send SIGNAL to foreground process in window matching MATCH."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Window: "))
         (completing-read "Signal: "
                          '("SIGTERM" "SIGKILL" "SIGINT" "SIGHUP" "SIGUSR1" "SIGUSR2"))))
  (kitty-rc-call "signal-child" "--match" match "--signal" signal))

;;; User variables

;;;###autoload
(defun kitty-rc-set-user-vars (match &rest vars)
  "Set user VARS on window matching MATCH.
VARS are key=value pairs."
  (interactive
   (list (kitty-rc--match-id (kitty-rc--read-window "Window: "))
         (split-string (read-string "Vars (key=value ...): "))))
  (apply #'kitty-rc-call "set-user-vars" "--match" match (flatten-list vars)))

;;; Convenience commands

;;;###autoload
(defun kitty-rc-select-window ()
  "Visually select a window in kitty."
  (interactive)
  (kitty-rc-call "select-window"))

;;;###autoload
(defun kitty-rc-focus-recent (n)
  "Focus Nth most recently active window."
  (interactive "p")
  (kitty-rc-call "focus-window" "--match" (kitty-rc--match-recent n)))

;;;###autoload
(defun kitty-rc-send-region (start end &optional match)
  "Send region from START to END to window matching MATCH.
If MATCH is nil, prompts for window selection."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (match (or match
                   (kitty-rc--match-id (kitty-rc--read-window "Send to: ")))))
    (kitty-rc-call "send-text" "--match" match text)))

;;;###autoload
(defun kitty-rc-send-region-to (start end match)
  "Send region from START to END to window matching MATCH pattern."
  (interactive "r\nsMatch pattern: ")
  (kitty-rc-send-region start end match))

;;;###autoload
(defun kitty-rc-copy-last-output ()
  "Copy last command output from active kitty window to kill ring."
  (interactive)
  (let ((text (kitty-rc-call "get-text" "--extent" "last_cmd_output")))
    (kill-new text)
    (message "Copied %d chars" (length text))))

;;; Claude Code integration

(defun kitty-rc--format-file-context ()
  "Format current file and selection as context for Claude.
Returns string with file path and optional code-fenced selection."
  (let* ((file (or buffer-file-name (buffer-name)))
         (relative-file (if buffer-file-name
                            (file-relative-name buffer-file-name (projectile-project-root))
                          file))
         (has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (mode-name (replace-regexp-in-string "-mode$" ""
                                              (symbol-name major-mode))))
    (concat relative-file
            (when has-region
              (concat "\n```" mode-name "\n"
                      region-text
                      (unless (string-suffix-p "\n" region-text) "\n")
                      "```")))))

(defun kitty-rc--resolve-claude-window (&optional current-tab-only no-prompt)
  "Get Claude window ID, prompting if multiple exist.
If CURRENT-TAB-ONLY, only search active tab.
If NO-PROMPT, use first (most recent) window without prompting."
  (let ((windows (kitty-rc--collect-claude-windows current-tab-only)))
    (cond
     ;; No Claude windows
     ((null windows)
      (user-error "No Claude Code windows found%s"
                  (if current-tab-only " in current tab" "")))
     ;; Single window - use it
     ((= (length windows) 1)
      (setq kitty-rc--last-claude-window (cdar windows)))
     ;; Multiple windows, no prompt requested - use MRU
     (no-prompt
      (setq kitty-rc--last-claude-window (cdar windows)))
     ;; Multiple windows - prompt (list already MRU sorted)
     (t
      (setq kitty-rc--last-claude-window
            (kitty-rc--read-claude-window "Claude window: " current-tab-only))))))

;;;###autoload
(defun kitty-rc-send-to-claude (&optional focus current-tab-only)
  "Send current file context to Claude Code window (MRU, no prompt).
With FOCUS non-nil, also focus the window.
With CURRENT-TAB-ONLY non-nil, only consider windows in active tab."
  (interactive (list t t))
  (let ((win-id (kitty-rc--resolve-claude-window current-tab-only 'no-prompt))
        (context (kitty-rc--format-file-context)))
    (kitty-rc-call "send-text" "--match" (kitty-rc--match-id win-id) context)
    (when focus
      (kitty-rc-call "focus-window" "--match" (kitty-rc--match-id win-id)))
    (message "Sent to Claude: %s" (car (split-string context "\n")))))

;;;###autoload
(defun kitty-rc-send-to-claude-all (&optional focus)
  "Send current file context to Claude Code window (search all tabs, MRU).
With FOCUS non-nil (default interactive), also focus the window."
  (interactive (list t))
  (let ((win-id (kitty-rc--resolve-claude-window nil 'no-prompt))
        (context (kitty-rc--format-file-context)))
    (kitty-rc-call "send-text" "--match" (kitty-rc--match-id win-id) context)
    (when focus
      (kitty-rc-call "focus-window" "--match" (kitty-rc--match-id win-id)))
    (message "Sent to Claude: %s" (car (split-string context "\n")))))

;;;###autoload
(defun kitty-rc-focus-claude (&optional current-tab-only)
  "Focus a Claude Code window (prompts if multiple exist).
With CURRENT-TAB-ONLY non-nil, only consider windows in active tab."
  (interactive (list t))
  (let ((win-id (kitty-rc--resolve-claude-window current-tab-only)))
    (kitty-rc-call "focus-window" "--match" (kitty-rc--match-id win-id))))

;;;###autoload
(defun kitty-rc-select-claude-window (&optional current-tab-only)
  "Interactively select a Claude window and set it as last used.
With CURRENT-TAB-ONLY non-nil, only show windows in active tab."
  (interactive "P")
  (setq kitty-rc--last-claude-window
        (kitty-rc--read-claude-window "Select Claude window: " current-tab-only))
  (message "Selected Claude window: %d" kitty-rc--last-claude-window))

(provide '+kitty-rc)
;;; +kitty-rc.el ends here
