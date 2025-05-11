;;; cli.el -*- lexical-binding: t; -*-

;; see: {doom-emacs-dir}/lisp/cli/env.el

(defcli! (:before env) ()
         (add-to-list 'doom-env-deny "SESSION_(ID|KEY|DESKTOP|PATH)$")
         (add-to-list 'doom-env-deny "^DESKTOP_(SESSION|STARTUP_ID)$")
         (add-to-list 'doom-env-deny "^DIRENV_WATCHES")
         (add-to-list 'doom-env-deny "^KITTY_")
         (add-to-list 'doom-env-deny "^LaunchInstanceId$")
         (add-to-list 'doom-env-deny "^SECURITYSESSION")
         (add-to-list 'doom-env-deny "^STARSHIP_")
         (add-to-list 'doom-env-deny "^WEZTERM_(PANE|UNIX_SOCKET)$")
         (add-to-list 'doom-env-deny "^WINDOWID$")
         (add-to-list 'doom-env-deny "^_$")
         (add-to-list 'doom-env-deny "^forgit_")

         (print! "doom-env-deny:")
         (dolist (env doom-env-deny)
           (print! "- %s" env))
         )

(defcli! (:after env) ()
         (print! "\n")
         (print! "doom-env-file (%s) contains:" doom-env-file)
         (dolist (env (sort (doom-file-read doom-env-file :by 'read :noerror t)))
           (print! "- %s" (car (split-string env "=" t)))))
