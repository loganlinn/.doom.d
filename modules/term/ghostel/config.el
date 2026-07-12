;;; term/ghostel/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :when (bound-and-true-p module-file-suffix)
  :commands (ghostel
             ghostel-list-buffers
             ghostel-next
             ghostel-other
             ghostel-previous
             ghostel-project
             ghostel-project-list-buffers
             ghostel-project-next
             ghostel-project-previous)
  :init
  ;; Keep the native module outside straight's package checkout, which can be
  ;; rebuilt while Emacs is still holding the module open.
  (setq ghostel-module-directory (expand-file-name "ghostel/" doom-data-dir))

  (map! :leader
        (:prefix-map ("o" . "open")
         :desc "Ghostel" "g" #'ghostel
         :desc "Ghostel buffer" "G" #'ghostel-list-buffers)
        (:prefix-map ("p" . "project")
         :desc "Ghostel" "m" #'ghostel-project
         :desc "Ghostel buffers" "M" #'ghostel-project-list-buffers))

  (after! evil-escape
    (add-to-list 'evil-escape-excluded-major-modes 'ghostel-mode))
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (when (modulep! :tools magit)
    (add-to-list 'ghostel-eval-cmds
                 '("magit-status-setup-buffer" magit-status-setup-buffer))))

(use-package! evil-ghostel
  :when (and (bound-and-true-p module-file-suffix)
             (modulep! :editor evil))
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package! ghostel-eshell
  :when (and (bound-and-true-p module-file-suffix)
             (modulep! :term eshell))
  :commands (eshell/ghostel ghostel-eshell-visual-command-mode)
  :hook (eshell-load . ghostel-eshell-visual-command-mode))
