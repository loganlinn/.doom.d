;; -*- no-byte-compile: t; -*-
;;; lyl.el

;; (package! aidermacs)

(package! gptel :recipe (:nonrecursive t))

(package! chatgpt-shell)
;; 
;; (package! dall-e-shell)

(when (modulep! +copilot)
  (package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el"))
  ;; (package! copilot-chat)
  )

(when (modulep! -copilot)
  (package! minuet))

(package! forge-llm
  :recipe (:host gitlab :repo "rogs/forge-llm"))

;; `magit-gptcommit' is much better.
(package! magit-gptcommit)
(disable-packages! gptel-magit)
