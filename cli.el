;;; cli.el -*- lexical-binding: t; -*-

(defcli! (:before env) ()
 (add-to-list 'doom-env-deny "SESSION_(ID|KEY|DESKTOP|PATH)$")
 (add-to-list 'doom-env-deny "^DESKTOP_STARTUP_ID$")
 (add-to-list 'doom-env-deny "^DESKTOP_SESSION$")
 (add-to-list 'doom-env-deny "^KITTY_")
 (add-to-list 'doom-env-deny "^STARSHIP_")
 (add-to-list 'doom-env-deny "^WINDOWID$")
 (add-to-list 'doom-env-deny "^forgit_$")

 ;; (add-to-list 'doom-env-allow )
 (print! "doom-env-deny :: %s" doom-env-deny)
 (print! "doom-env-allow :: %s" doom-env-allow))
