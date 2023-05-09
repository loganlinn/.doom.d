;;; app/openai/config.el -*- lexical-binding: t; -*-

(use-package! openai
  :defer t
  :config
  (setq! openai-key #'openai-key-auth-source))

(use-package! codegpt
  :commands (codegpt-mode
             codegpt-doc
             codegpt-explain
             codegpt-fix
             codegpt-improve))

(use-package! chatgpt-shell
  :commands (chatgpt-shell
             dall-e-shell)
  :init
  (setq! chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))
         chatgpt-shell-chatgpt-streaming t))
