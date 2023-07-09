;;; +elisp.el -*- lexical-binding: t; -*-

(add-hook! 'emacs-lisp-mode-hook #'+loganlinn/lisp-coding-defaults)

(add-hook! 'lisp-mode-hook #'+loganlinn/lisp-coding-defaults)

(add-hook! 'lisp-data-mode-hook #'+loganlinn/lisp-coding-defaults)

(after! ielm
  (set-popup-rule! "^\\*ielm\\*$" :vslot 2 :size 0.3 :quit nil :ttl nil)

  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'inferior-emacs-lisp-mode))

  (defun emacs-lisp/switch-to-last-elisp-buffer ()
    (interactive)
    (if-let* ((buf (seq-find (lambda (b)
                               (with-current-buffer b
                                 (derived-mode-p 'emacs-lisp-mode)))
                             (buffer-list))))
        (pop-to-buffer buf)
      (switch-to-prev-buffer)))

  ;; similar binding to to cider-mode <> cider-repl-mode
  (map! (:map emacs-lisp-mode-map
              "C-c C-z" #'+emacs-lisp/open-repl)
        (:map inferior-emacs-lisp-mode-map
              "C-c C-z" #'emacs-lisp/switch-to-last-elisp-buffer)))
