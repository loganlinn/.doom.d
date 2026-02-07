;;; term/kitty/autoload/kitty.el -*- lexical-binding: t; -*-

;;; Tabs

;;;###autoload
(defun +kitty/new-tab (&optional title)
  "Open a new kitty tab with optional TITLE. Inherits oldest cwd."
  (interactive "sTab title: ")
  (apply #'+kitty--run-async
         (append '("launch" "--type=tab" "--cwd=oldest" "--location=after")
                 (when (and title (not (string-empty-p title)))
                   (list "--title" title)))))

;;;###autoload
(defun +kitty/focus-tab (n)
  "Focus kitty tab N (1-indexed)."
  (interactive "nTab number: ")
  (+kitty--run-async "focus-tab" "--match" (format "index:%d" (1- n))))

;;;###autoload
(defun +kitty/set-tab-title (title)
  "Set the current kitty tab TITLE."
  (interactive "sTab title: ")
  (+kitty--run-async "set-tab-title" title))

;;;###autoload
(defun +kitty/next-tab ()
  "Focus next kitty tab."
  (interactive)
  (+kitty--run-async "focus-tab" "--match" "recent:1"))

;;; Windows

;;;###autoload
(defun +kitty/new-window ()
  "Open a new kitty window (split) inheriting oldest cwd."
  (interactive)
  (+kitty--run-async "launch" "--type=window" "--cwd=oldest" "--location=split"))

;;;###autoload
(defun +kitty/close-window ()
  "Close the active kitty window."
  (interactive)
  (+kitty--run-async "close-window"))

;;;###autoload
(defun +kitty/focus-window (direction)
  "Focus neighboring kitty window in DIRECTION (left/right/up/down)."
  (interactive (list (completing-read "Direction: " '("left" "right" "up" "down"))))
  (+kitty--run-async "focus-window" "--match" (format "neighbor:%s" direction)))

;;; Layouts

;;;###autoload
(defun +kitty/set-layout (layout)
  "Set kitty LAYOUT for current tab."
  (interactive (list (completing-read "Layout: " +kitty-layouts)))
  (+kitty--run-async "goto-layout" layout))

;;;###autoload
(defun +kitty/zoom-toggle ()
  "Toggle between stack and previous layout (mirrors zoom_toggle.py)."
  (interactive)
  (+kitty--run-async "goto-layout" "stack"))

;;;###autoload
(defun +kitty/next-layout ()
  "Cycle to next kitty layout."
  (interactive)
  (+kitty--run-async "next-layout"))

;;; Opacity

;;;###autoload
(defun +kitty/set-opacity (opacity)
  "Set kitty background OPACITY (0.0-1.0, or +/- delta)."
  (interactive "sOpacity (0.0-1.0 or +/-delta): ")
  (+kitty--run-async "set-background-opacity" opacity))

;;;###autoload
(defun +kitty/opacity-increase ()
  "Increase kitty background opacity by 0.1."
  (interactive)
  (+kitty/set-opacity "+0.1"))

;;;###autoload
(defun +kitty/opacity-decrease ()
  "Decrease kitty background opacity by 0.1."
  (interactive)
  (+kitty/set-opacity "-0.1"))

;;;###autoload
(defun +kitty/opacity-reset ()
  "Reset kitty background opacity to default."
  (interactive)
  (+kitty/set-opacity "default"))

;;; Launch

;;;###autoload
(defun +kitty/yazi (&optional dir)
  "Open yazi file manager in a kitty overlay at DIR or current directory."
  (interactive)
  (let ((d (or dir default-directory)))
    (+kitty--run-async "launch" "--type=overlay" "--cwd" d "--title" "files" "yazi")))

;;;###autoload
(defun +kitty/open-in-window (file)
  "Open FILE in a new kitty os-window with $EDITOR."
  (interactive "fFile: ")
  (+kitty--run-async "launch" "--type=os-window" "--"
                     (or (getenv "EDITOR") "vim") (expand-file-name file)))

;;;###autoload
(defun +kitty/overlay-cmd (cmd &rest args)
  "Run CMD with ARGS in a kitty overlay window."
  (+kitty--run-async "launch" "--type=overlay" "--cwd=current" "--hold" cmd))

;;;###autoload
(defun +kitty/send-text (text &optional match)
  "Send TEXT to kitty window (optionally matching MATCH)."
  (interactive "sText: ")
  (apply #'+kitty--run-async
         (append '("send-text")
                 (when match (list "--match" match))
                 (list "--" text))))

;;; Scrollback & hints

;;;###autoload
(defun +kitty/scroll-to-prompt (direction)
  "Scroll to next/previous shell prompt. DIRECTION: 1 (next) or -1 (previous)."
  (interactive (list (if current-prefix-arg 1 -1)))
  (+kitty--run-async "scroll-window" (number-to-string direction)))

;;;###autoload
(defun +kitty/last-command-output ()
  "Show the output of the last command in kitty."
  (interactive)
  (+kitty--run-async "show-last-command-output"))

;;;###autoload
(defun +kitty/grab ()
  "Launch kitty_grab for visual selection in scrollback."
  (interactive)
  (+kitty--run-async "kitten" "kitty_grab/grab.py"))

;;;###autoload
(defun +kitty/hints (type &optional program)
  "Show kitty hints of TYPE, using PROGRAM to handle selection.
TYPE: path, url, hash, word, line, linenum, ip, hyperlink.
PROGRAM: - (paste), @ (clipboard), default (open)."
  (interactive (list (completing-read "Hint type: "
                                      '("path" "url" "hash" "word" "line"
                                        "linenum" "ip" "hyperlink"))
                     (completing-read "Action: " '("-" "@" "default"))))
  (apply #'+kitty--run-async
         (append (list "kitten" "hints" "--type" type)
                 (when program (list "--program" program)))))

;;; Clipboard

;;;###autoload
(defun +kitty/yank-cwd ()
  "Copy current working directory to system clipboard via kitty."
  (interactive)
  (let ((dir (or default-directory "~")))
    (+kitty--run "send-text" "--match=self" "--"
                 (format "printf '%%s' '%s' | kitten clipboard /dev/stdin\n"
                         (expand-file-name dir)))))

;;; Sessions

;;;###autoload
(defun +kitty/load-session (session)
  "Load a kitty SESSION file."
  (interactive
   (list (completing-read "Session: "
                          (directory-files +kitty-sessions-dir nil
                                           "\\.kitty-session$"))))
  (+kitty--run-async "goto-session"
                     (expand-file-name session +kitty-sessions-dir)))

;;;###autoload
(defun +kitty/save-session ()
  "Save current kitty session."
  (interactive)
  (+kitty--run "save-as-session" "--relocatable" "--use-foreground-process"
               "--match=session:."))

;;; Markers

;;;###autoload
(defun +kitty/create-marker (type text)
  "Create a marker matching TEXT with TYPE (text or regex)."
  (interactive (list (completing-read "Type: " '("text" "regex"))
                     (read-string "Pattern: ")))
  (let ((match-case (if (string= text (downcase text)) "i" "")))
    (+kitty--run-async "create-marker" (concat match-case type) "1" text)))

;;;###autoload
(defun +kitty/remove-marker ()
  "Remove markers from current kitty window."
  (interactive)
  (+kitty--run-async "remove-marker"))

;;; Signals

;;;###autoload
(defun +kitty/signal (sig)
  "Send signal SIG to the foreground process in current kitty window."
  (interactive (list (completing-read "Signal: "
                                      '("SIGTERM" "SIGKILL" "SIGQUIT" "SIGHUP" "SIGTSTP"))))
  (+kitty--run-async "signal-child" sig))

;;; Info

;;;###autoload
(defun +kitty/ls ()
  "Return parsed JSON of kitty state (windows, tabs, etc)."
  (interactive)
  (let ((output (+kitty--run "ls")))
    (when output
      (let ((parsed (json-parse-string output :object-type 'alist)))
        (when (called-interactively-p 'any)
          (with-current-buffer (get-buffer-create "*kitty-ls*")
            (erase-buffer)
            (insert output)
            (json-mode)
            (pop-to-buffer (current-buffer))))
        parsed))))

;;;###autoload
(defun +kitty/set-font-size (delta)
  "Change kitty font size by DELTA (+2/-2/0 to reset)."
  (interactive "nFont size delta (0 to reset): ")
  (+kitty--run-async "set-font-size" "--" (number-to-string delta)))
