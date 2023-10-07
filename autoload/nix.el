;;; autoload/nix.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +nix/switch-to-last-nix-buffer ()
  (interactive)
  (if-let* ((buf (seq-find (lambda (b)
                             (with-current-buffer b
                               (and (derived-mode-p 'nix-mode)
                                    (not (derived-mode-p 'nix-repl-mode)))))
                           (buffer-list))))
      (pop-to-buffer buf)
    (switch-to-prev-buffer)))

;;;###autoload
(defun +nix/switch-to-repl-buffer ()
  (interactive)
  (nix-repl)
  (goto-char (point-max)))

;;;###autoload
(defun +nix/set-repl-file (&optional file)
  (interactive "Fnix repl file: ")
  (setq nix-repl-executable-args (list "repl" "--file" file)))

;;;###autoload
(defun +nix/add-repl-expr (&optional expr)
  (interactive (read-string "Set Nix REPL expression: "))
  (setq nix-repl-executable-args (append nix-repl-executable-args (list "--expr" expr))))

;;;###autoload
(defun +nix/detect-repl-file ()
  (interactive)
  (if-let ((root (projectile-locate-dominating-file default-directory "repl.nix")))
      (+nix/set-repl-file (file-name-concat root "repl.nix"))
    (if-let ((root (projectile-locate-dominating-file default-directory "shell.nix")))
        (+nix/set-repl-file (file-name-concat root "shell.nix"))
      (user-error "No repl.nix or shell.nix detected"))))

;;;###autoload
(defun +nix/alejandra (&optional output-buffer error-buffer display-error-buffer)
  (interactive (list current-prefix-arg shell-command-default-error-buffer t))
  (shell-command-on-region
   (point-min) (point-max)
   "nix run nixpkgs#alejandra --quiet -"
   (or output-buffer (current-buffer))
   t
   error-buffer
   display-error-buffer))

;;;###autoload
(defun +nix/nixpkgs-fmt (&optional output-buffer error-buffer display-error-buffer)
  (interactive (list current-prefix-arg shell-command-default-error-buffer t))
  (shell-command-on-region
   (point-min) (point-max)
   "nix run nixpkgs#nixpkgs-fmt --quiet -"
   (or output-buffer (current-buffer))
   t
   error-buffer
   display-error-buffer))
