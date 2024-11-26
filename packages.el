;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! doom-themes
  :recipe (:host github :repo "loganlinn/doom-themes")
  :pin "e47e604290ffe16b01e64c585d7c21c4b2a9d94a")

(package! aggressive-indent)
;; (package! bicycle :recipe (:host github :repo "tarsius/bicycle"))
(package! button-lock)
(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))
(package! crux :recipe (:host github :repo "bbatsov/crux") :pin "f8789f67a9d2e1eb31a0e4531aec9bb6d6ec1282" :disable t)
(package! docker-compose-mode)
(package! evil-cleverparens :recipe (:host github :repo "emacs-evil/evil-cleverparens") :pin "134fe3396f975fb1ad261f52cd113f42ac1da4fc")
(package! fence-edit :recipe (:host github :repo "aaronbieber/fence-edit.el") :pin "fab7cee16e91c2d8f9c24e2b08e934fa0813a774")
(package! gh-notify :disable t)
(package! graphviz-dot-mode :disable t)
(package! highlight-parentheses)
(package! keycast)
(package! kubel :disable t)
(package! kubel-evil :disable t)
(package! llamma :recipe (:host github :repo "tarsius/llama") :disable t)
(package! mermaid-mode)
(package! minions)
(package! ob-mermaid :disable t)
(package! obsidian :disable t)
(package! ox-gist :recipe (:host github :repo "punchagan/ox-gist") :pin "e9f1f11af0e97fee30c2b15b56c236b1f4e1f400")
(package! outshine)
(package! parseclj)
(package! parseedn)
(package! prisma-mode :recipe (:host github :repo "pimeys/emacs-prisma-mode" :branch "main"))
(package! protobuf-mode :recipe (:host github :repo "protocolbuffers/protobuf" :files ("editors/protobuf-mode.el")) :pin "28e573e77fc3b453dd242e3848b19e7adbf04984" :disable t)
(package! v-mode :disable t)
(package! vala-mode :disable t)
(package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe") :pin "db9fbc95bb8316165ec74e500a76d6857e6ced1a" :disable t)
(package! yuck-mode :disable t)
(package! zprint-mode :disable t)

;;; ~/.doom.d/packages.el ends here
