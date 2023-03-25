;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; (package! benchmark-init)

;;; :editor evil
;; (package! evil-string-inflection) ;; evil operator to cycle *-case in text objects

;;; :tools lookup 
(package! synosaurus) ;; https://github.com/doomemacs/doomemacs/issues/6076

;;; :tools magit
; (package! magit-delta)

;;; :ui
(package! button-lock)

;;; :lang clojure, emacs-lisp
(package! aggressive-indent)
(package! evil-cleverparens)
;;(package! evil-smartparens)
(package! zprint-mode)

;;; :lang mermaid
(package! mermaid-mode)
(package! ob-mermaid)

;;; :lang yaml
;; (package! k8s-mode :recipe
;;   (:host github
;;    :repo "loganlinn/emacs-k8s-mode"))

;;; :lang yuck
(package! yuck-mode)

;;; :term
; (package! term-keys :recipe (:host github :repo "CyberShadow/term-keys" :files ("*.el")))

;; FIXME needed? if so, document why...
(unpin! lsp-treemacs)
(unpin! lsp-ui)
(unpin! treemacs)
(unpin! hover)
;; (package! flymake-shellcheck)
;; (package! just-mode)
;; (package! justl)

;; https://emacs.stackexchange.com/questions/75827/doom-emacs-error-running-hook-global-git-commit-mode-because-void-variable
(package! transient
      :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
      :recipe (:host github :repo "magit/transient"))

(package! with-editor
          :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
          :recipe (:host github :repo "magit/with-editor"))

;;; :lang v
(package! v-mode)

;;; ~/.doom.d/packages.el ends here
