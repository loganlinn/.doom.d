;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; tools/codex-ide/packages.el

(package! codex-ide
  :recipe (:host github
           :repo "dgillis/emacs-codex-ide"
           :files ("*.el" "bin"))
  :pin "13cd85a8f2e7b196ae73e497f9b5e4a38a581af4")
