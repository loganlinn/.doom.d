;;; term/kitty/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "kitty")
  (warn! "Couldn't find kitty executable"))

(unless (getenv "KITTY_WINDOW_ID")
  (warn! "Not running inside kitty (KITTY_WINDOW_ID unset)"))

(when (and (getenv "KITTY_WINDOW_ID")
           (not (or (getenv "KITTY_LISTEN_ON")
                    (getenv "KITTY_PID"))))
  (warn! "Kitty remote control socket not detected; ensure `allow_remote_control' and `listen_on' are configured"))

(unless (zerop (call-process "kitty" nil nil nil "@" "ls"))
  (warn! "kitty @ ls failed; remote control may not be enabled"))
