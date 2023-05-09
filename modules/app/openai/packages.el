;; -*- no-byte-compile: t; -*-
;;; app/openai/packages.el

(package! openai
  :pin "6a1d270fcdf89226f22122df930afe17f85d3209"
  :recipe (:type git :host github :repo "emacs-openai/openai"))

(package! chatgpt
  :pin "73a1d192b2eae9fb65e0688d7c99e91bc4101d38"
  :recipe (:type git :host github :repo "emacs-openai/chatgpt"))

(package! codegpt
  :pin "d5de204b6438eafeaa667d3007699f84ac87f5f9"
  :recipe (:type git :host github :repo "emacs-openai/codegpt"))

(package! chatgpt-shell)
