;;; +lua.el -*- lexical-binding: t; -*-

(when (modulep! :lang lua +lsp)
  (after! lsp-lua
    (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server")
          lsp-clients-lua-lsp-server-install-dir lsp-clients-lua-language-server-bin
          ;; lsp-clients-lua-language-server-main-location "/opt/lua-language-server/main.lua"
          lsp-lua-hint-enable t
          lsp-lua-hint-set-type t))
  ;; automatically start fennel-ls
  (after! fennel-mode
    (add-hook 'fennel-mode-local-vars-hook #'lsp! 'append)))
