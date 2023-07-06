;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; (package! benchmark-init)

;;; :editor evil
;; (package! evil-string-inflection) ;; evil operator to cycle *-case in text objects

;;; :tools magit
; (package! magit-delta)

;;; :ui
(package! button-lock)

;;; :tools magit
(package! gh-notify)

;;; :lang clojure, emacs-lisp
(package! aggressive-indent)
(package! evil-cleverparens)
(package! zprint-mode)

;;; :lang dot
(package! graphviz-dot-mode)

;;; :lang mermaid
;; (package! mermaid-mode)
;; (package! ob-mermaid)

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
