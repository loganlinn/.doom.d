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
(defun +nix/add-repl-expr (&optional expr)
  (interactive (list (read-string "Set Nix REPL expression: ")))
  (setq nix-repl-executable-args
   (append nix-repl-executable-args `("--expr" ,expr))))

;;;###autoload
(defun +nix/add-repl-file (&optional file)
  (interactive (list (read-file-name "Set Nix REPL file: ")))
  (setq nix-repl-executable-args
   (append nix-repl-executable-args `("--file" ,file))))
