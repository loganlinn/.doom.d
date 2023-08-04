;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; (package! benchmark-init)

;;; :completion vertico
;; NOTE: see other NOTE in init.el.
(package! vertico-posframe
  :recipe (:host github :repo "tumashu/vertico-posframe")
  :pin "7da6d648ff4202a48eb6647ee7dce8d65de48779")

;;; :editor evil
;; (package! evil-string-inflection) ;; evil operator to cycle *-case in text objects

;;; :tools magit
;; (package! magit-delta)

;;; :ui
(package! button-lock)

;;; :tools magit
(package! gh-notify)

;; ;;; :lang asciidoc
;; (package! adoc-mode)

;;; :lang lisp
(package! aggressive-indent)
(package! evil-cleverparens
  :recipe (:host github :repo "emacs-evil/evil-cleverparens")
  :pin "9ee249509281c387500e397df625ccb759804df4")
(package! zprint-mode)
(package! parseedn)
(package! parseclj)
(package! highlight-parentheses)

;;; :lang dot
(package! graphviz-dot-mode)

;;; :lang mermaid
;; (package! mermaid-mode)
;; (package! ob-mermaid)

;;; :lang nix +lsp
(package! nix-mode :pin "719feb7868fb567ecfe5578f6119892c771ac5e5") ;; (newer than doom module pin)

;;; :lang protobuf
(package! protobuf-mode)

;;; :lang yuck
(package! yuck-mode)

;;; :lang just
(package! just-mode)
(package! justl)

;;; :lang viml
;; (package! vimrc-mode)

;; https://emacs.stackexchange.com/questions/75827/doom-emacs-error-running-hook-global-git-commit-mode-because-void-variable
;; (package! transient
;;       :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
;;       :recipe (:host github :repo "magit/transient"))
;;
;; (package! with-editor
;;           :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
;;           :recipe (:host github :repo "magit/with-editor"))

;;; :lang v
;; (package! v-mode)

;;; :lang vala
;; (package! vala-mode)

;;; ~/.doom.d/packages.el ends here
