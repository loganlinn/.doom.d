;;; autoload/kitty.el -*- lexical-binding: t; -*-

(defvar kitty-term-command "kitty")

(add-to-list 'display-buffer-alist '("*kitty-term*" display-buffer-no-window (nil)))

;;;###autoload
(defun kitty-term ()
  "Starts new kitty terminal process"
  (interactive)
  (funcall-interactively #'async-shell-command kitty-term-command "*kitty-term*" "*kitty-term*"))
