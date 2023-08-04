;;; +nix.el -*- lexical-binding: t; -*-

(after! nix
  (map! :map nix-mode-map
        :ni "C-c C-z" #'+nix/switch-to-repl-buffer
        (:localleader
         "p" #'+format/region-or-buffer ;; replace nix-mode's format binding with that from doom-code-map (SPC c), i.e. format-all module
         "r" #'+nix/switch-to-repl-buffer
         "f" #'nix-flake
         ))

  (add-hook! 'nix-mode-hook
    (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))
    (set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes '(nix-mode))

    (setq +format-with 'nixpkgs-fmt
          +format-with-lsp nil)

    (quiet! (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

    (nix-prettify-turn-on)

    ;; configure repl to use repl.nix at root of project detect a repl.nix file at project root
    (let ((repl-file (doom-project-expand "repl.nix")))
      (when (file-exists-p repl-file)
        (message "Set nix-repl to use" repl-file)
        (+nix/add-repl-file repl-file))))

  ;;; nix-repl-mode

  (add-hook! 'nix-repl-mode-hook
    (nix-prettify-turn-on))

  ;;; Nix REPL
  (set-popup-rule! "^\\*Nix-REPL" :size 0.3 :quit nil :select t)

  (when (modulep! :editor evil)
    (add-to-list 'evil-insert-state-modes 'nix-repl-mode))

  (map! :map nix-repl-mode-map
        :ni "C-c C-z" #'+nix/switch-to-last-nix-buffer
        :ni "C-p" #'comint-previous-input
        :ni "C-n" #'comint-next-input
        :ni "C-r" #'comint-history-isearch-backward-regexp
        :ni "C-l" #'comint-clear-buffer
        :ni "C-u" #'comint-kill-input
        :i "C-d" #'delete-forward-char
        :n "RET" #'comint-send-input))
