;;; +mise.el -*- lexical-binding: t; -*-

(use-package! mise
  :defer t
  :hook (doom-after-init . global-mise-mode)
  ;; :commands (mise-reload
  ;;            mise-update-buffer
  ;;            mise-update-dir)
  :init
  (setq mise-log-level 'info)
  :config
  (mise-setup-default))
