;;; term/kitty/config.el -*- lexical-binding: t; -*-

;;; Kitty terminal integration for Doom Emacs.
;;
;; Provides remote control commands and terminal detection when running inside
;; kitty. Mirrors keybinding patterns and workflows from kitty config.

(defvar +kitty-socket nil
  "Path to the kitty remote control socket.
Auto-detected from $KITTY_LISTEN_ON or constructed from $KITTY_PID.")

(defvar +kitty-layouts '("tall" "fat" "grid" "horizontal" "splits" "stack" "vertical")
  "Available kitty layouts (matching `enabled_layouts' config).")

(defvar +kitty-sessions-dir (expand-file-name "~/.config/kitty/")
  "Directory containing kitty session files.")

(defvar +kitty-remote-control-p nil
  "Non-nil when kitty remote control is available.")

;;; Detection & setup

(defun +kitty--detect ()
  "Detect kitty environment and configure remote control socket."
  (when (getenv "KITTY_WINDOW_ID")
    (setq +kitty-socket
          (or (getenv "KITTY_LISTEN_ON")
              (when-let ((pid (getenv "KITTY_PID")))
                (format "unix:kitty-socket.%s" pid))))
    (setq +kitty-remote-control-p (and +kitty-socket t))))

(+kitty--detect)

;;; Terminal compatibility

;; xterm-kitty terminfo support
(when (and (not (display-graphic-p))
           (string-prefix-p "xterm-kitty" (or (getenv "TERM") "")))
  ;; Ensure Emacs treats kitty's TERM properly
  (unless (assoc "xterm-kitty" term-file-aliases)
    (push '("xterm-kitty" . "xterm") term-file-aliases)))

;;; Remote control helpers

(defun +kitty--run (&rest args)
  "Run `kitty @' with ARGS. Returns stdout on success, nil on failure."
  (unless +kitty-remote-control-p
    (user-error "Kitty remote control not available"))
  (let ((cmd (append (list "kitty" "@" "--to" +kitty-socket) args)))
    (with-temp-buffer
      (if (zerop (apply #'call-process (car cmd) nil t nil (cdr cmd)))
          (string-trim (buffer-string))
        (message "kitty @: %s" (string-trim (buffer-string)))
        nil))))

(defun +kitty--run-async (&rest args)
  "Run `kitty @' with ARGS asynchronously (fire-and-forget)."
  (unless +kitty-remote-control-p
    (user-error "Kitty remote control not available"))
  (apply #'start-process "kitty" nil "kitty" "@" "--to" +kitty-socket args))
