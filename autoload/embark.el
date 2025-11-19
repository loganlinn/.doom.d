;;; +embark.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +embark:find-file-h (target)
  (split-window-below)
  (other-window 1)
  (find-file target))

;;;###autoload
(defun +embark:split-vertical-current-completion-candidate ()
  (interactive)
  (embark--act #'find-file-other-window (car (embark--targets)) t))

;;;###autoload
(defun +embark:split-horizontal-current-completion-candidate ()
  (interactive)
  (embark--act #'+embark:find-file-h (car (embark--targets)) t))
