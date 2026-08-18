;;; +markdown.el -*- lexical-binding: t; -*-

;; Replace Doom's GitHub-light preview preamble (CDN CSS + highlight.js) with
;; a local dark stylesheet tuned for coding-agent plan documents. Pandoc
;; (`+markdown-compile-pandoc') already emits classed syntax spans, so no
;; client-side highlighter is needed. Palettes live in markdown-preview.css.
(setq markdown-css-paths
      (list (concat "file://" (expand-file-name "markdown-preview.css" doom-user-dir)))
      markdown-xhtml-header-content
      (concat "<meta name='viewport' content='width=device-width, initial-scale=1'>"
              "<meta name='color-scheme' content='dark'>"
              ;; pandoc runs with --mathjax; render math spans if present
              "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"))
