;;; lang/cue/config.el -*- lexical-binding: t; -*-

(use-package! cue-mode
  :defer t
  :config
  (let ((prefix (when (and (not (executable-find "cue"))
                          (executable-find "mise"))
                  '("mise" "exec" "cue" "--"))))
    (set-eglot-client! 'cue-mode (append prefix '("cue" "lsp" "serve"))))
  (when (modulep! +lsp)
    (add-hook 'cue-mode-local-vars-hook #'lsp! 'append)))
