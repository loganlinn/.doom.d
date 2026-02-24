;;; lang/pkl/config.el -*- lexical-binding: t; -*-

(use-package! pkl-mode
  :defer t
  :config
  (when (modulep! :lyl ai +copilot)
    (setq pkl-enable-copilot t)))
