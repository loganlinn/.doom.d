;;; lang/cue/config.el -*- lexical-binding: t; -*-

(use-package! cue-mode
  :defer t
  :config
  (set-eglot-client! 'cue-mode '("cue" "lsp" "serve"))
  (when (modulep! +lsp)
    (add-hook 'cue-mode-local-vars-hook #'lsp! 'append)))
