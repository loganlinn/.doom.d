;;; +nix.el -*- lexical-binding: t; -*-

;;; Nix REPL
(set-popup-rule! "^\\*Nix-REPL" :size 0.3 :quit nil :select t)

(after! nix
  ;; various nix formatters
  (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))
  (set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes '(nix-mode))

  ;; replace nix-mode's format binding with that from doom-code-map (SPC c), i.e. format-all module
  (map! :map nix-mode-map :localleader "p" #'+format/region-or-buffer)

  (setq-hook! 'nix-mode-hook
    +format-with 'nixpkgs-fmt
    +format-with-lsp nil)

  ;;; Nix REPL

  ;; configure repl to use repl.nix at root of project detect a repl.nix file at project root
  (add-hook! 'nix-mode-hook
    (let ((repl-file (doom-project-expand "repl.nix")))
      (when (file-exists-p repl-file)
        (+nix/add-repl-file repl-file))))

  (when (modulep! :editor evil)
   (add-to-list 'evil-insert-state-modes 'nix-repl-mode))

  ;; when switching to REPL, move cursor to prompt
  (advice-add 'nix-repl-show :after #'comint-goto-process-mark)

  ;; similar binding to to cider-mode <> cider-repl-mode
  (map! (:map nix-mode-map "C-c C-z" #'nix-repl-show)
        (:map nix-repl-mode-map "C-c C-z" #'+nix/switch-to-last-nix-buffer))

  (map! :map nix-repl-mode-map
        :ni "C-p" #'comint-previous-input
        :ni "C-n" #'comint-next-input
        :ni "C-r" #'comint-history-isearch-backward-regexp
        :ni "C-l" #'comint-clear-buffer
        :ni "C-u" #'comint-kill-input
        :n "RET" #'comint-send-input))
