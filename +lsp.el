;;; +lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 8264
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t)

  ;; completion
  ;; (setq lsp-completion-enable t
  ;;       lsp-completion-enable-additional-text-edit
  ;;       lsp-completion-filter-on-incomplete
  ;;       lsp-completion-no-cache
  ;;       lsp-completion-provider
  ;;       lsp-completion-show-detail t
  ;;       lsp-completion-show-kind t
  ;;       lsp-completion-show-label-description
  ;;       lsp-completion-sort-initial-results
  ;;       lsp-completion-use-last-result
  ;;       lsp-enable-snippet t)

  ;; lens
  (setq lsp-lens-enable t
        lsp-lens-place-position  #'end-of-line)

  ;; headerline
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; modeline
  (setq lsp-modeline-workspace-status-enable t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :file)

  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.terraform\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]\\.devenv\\'"
                 "[/\\\\]\\.direnv\\'"
                 "[/\\\\]\\.go\\'"
                 "[/\\\\]\\.env\\'"
                 "[/\\\\]\\.local\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]node_modules\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"))
    (push dir lsp-file-watch-ignored-directories)))

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-doc-border (doom-color 'fg)
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 100
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t))

(after! lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))
