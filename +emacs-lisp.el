;;; +elisp.el -*- lexical-binding: t; -*-

(after! ielm
  (set-popup-rule! "^\\*ielm\\*$" :vslot 2 :size 0.3 :quit nil :ttl nil)

  (when (modulep! :editor evil)
    (set-evil-initial-state! 'inferior-emacs-lisp-mode 'insert))

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
