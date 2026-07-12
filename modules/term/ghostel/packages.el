;; -*- no-byte-compile: t; -*-
;;; term/ghostel/packages.el

(package! ghostel)

(when (modulep! :editor evil)
  (package! evil-ghostel))
