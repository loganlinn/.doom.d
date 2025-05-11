;;; +llm.el -*- lexical-binding: t; -*-

(after! gptel
  (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)
  )
