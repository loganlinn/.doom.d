;;; tools/codex-ide/config.el -*- lexical-binding: t; -*-

(use-package! codex-ide
  :commands (codex-ide codex-ide-menu)
  :init
  (map! :leader
        (:prefix-map ("o" . "open")
         :desc "Codex IDE" "c" #'codex-ide-menu)))
