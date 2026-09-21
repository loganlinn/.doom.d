;;; +mise.el -*- lexical-binding: t; -*-

(use-package! mise
  :hook (doom-after-init . global-mise-mode)
  :init
  ;; Bootstrap mise independently of Emacs's inherited PATH.
  (setq mise-executable
        (expand-file-name "~/.dotfiles/bin/mise")))
