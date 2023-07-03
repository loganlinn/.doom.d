;;; +nix.el -*- lexical-binding: t; -*-

;;; Nix REPL
(set-popup-rule! "^\\*Nix-REPL" :size 0.3 :quit nil :select t)

(after! nix
  ;; start repl in insert mode
  (when (modulep! :editor evil)
   (add-to-list 'evil-insert-state-modes 'nix-repl-mode))

  ;; various nix formatters
  (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))
  (set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes '(nix-mode))

  ;; replace nix-mode's format binding with that from doom-code-map (SPC c), i.e. format-all module
  (map! :map nix-mode-map :localleader "p" #'+format/region-or-buffer)

  (setq-hook! 'nix-mode-hook
    +format-with 'nixpkgs-fmt
    +format-with-lsp nil)

  ;; repl
  (add-hook! 'nix-mode-hook
    (let ((repl-file (doom-project-expand "repl.nix")))
      (when (file-exists-p repl-file)
        (+nix/add-repl-file repl-file))))

  ;; similar binding to to cider-mode <> cider-repl-mode
  (map! (:map nix-mode-map "C-c C-z" #'nix-repl-show)
        (:map nix-repl-mode-map "C-c C-z" #'+nix/switch-to-last-nix-buffer)))
