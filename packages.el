;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; :tools lookup 
(package! synosaurus) ;; https://github.com/doomemacs/doomemacs/issues/6076

;;; :tools magit
(package! magit-delta)

;;; :ui
(package! button-lock)

;;; :lang clojure, emacs-lisp
(package! aggressive-indent)
(package! evil-cleverparens)
(package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))
(package! zprint-mode)

;;; :lang yaml
(package! k8s-mode :recipe
  (:host github
   :repo "loganlinn/emacs-k8s-mode"))

;; FIXME needed? if so, document why...
(unpin! lsp-treemacs)
(unpin! lsp-ui)
(unpin! treemacs)
(unpin! hover)
;; (package! flymake-shellcheck)
;; (package! just-mode)
;; (package! justl)

;;; ~/.doom.d/packages.el ends here
