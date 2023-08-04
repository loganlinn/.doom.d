;;; autoload/lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun loganlinn/lisp-coding-defaults ()
  (interactive)
  (evil-cleverparens-mode +1)
  (evil-cp-set-additional-bindings)
  (turn-on-smartparens-strict-mode)
  ;(smartparens-global-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (aggressive-indent-mode +1))
