;;; +nix.el -*- lexical-binding: t; -*-

(map! :after nix-mode
      :map nix-mode-map
      :localleader
      "p" #'+format/region-or-buffer ;; replace nix-mode's format binding with that from doom-code-map (SPC c), i.e. format-all module
      "f" #'nix-flake)

(map! :after nix-repl
      :map nix-mode-map
      :ni "C-c C-z" #'+nix/switch-to-repl-buffer
      :localleader
      "r" #'+nix/switch-to-repl-buffer)

(map! :after nix-repl
      :map nix-repl-mode-map
      :ni "C-c C-z" #'+nix/switch-to-last-nix-buffer
      :ni "C-p"     #'comint-previous-input
      :ni "C-n"     #'comint-next-input
      :ni "C-r"     #'comint-history-isearch-backward-regexp
      :ni "C-l"     #'comint-clear-buffer
      :ni "C-u"     #'comint-kill-input
      :i "C-d"      #'delete-forward-char
      :n "RET"      #'comint-send-input)

(after! nix-mode
  (add-hook! 'nix-mode-hook (nix-prettify-mode t)))

(after! nix-repl
  (set-popup-rule! "^\\*Nix-REPL" :size 0.3 :quit nil :select t)

  (add-hook! 'nix-repl-mode-hook (nix-prettify-mode t))

  (add-hook! 'nix-repl-mode-hook
    (let ((repl-file (doom-project-expand "repl.nix")))
      (when (file-exists-p repl-file)
        (setq-local nix-repl-executable-args (list "repl" "--file" repl-file))
        (print! "Configured nix-repl file: %s" repl-file)))))

(after! lsp-nix
  (setq lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(after! (:and evil nix-repl)
  (set-evil-initial-state! 'nix-repl-mode 'insert))
