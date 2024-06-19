;;; +vertico.el -*- lexical-binding: t; -*-


(use-package! vertico-posframe
  :when (modulep! +childframe)
  :after vertico ;; NB: previously used `:after-call vertico', but try less eager:
  :config

  ;; appearance
  (setq vertico-posframe-size-function #'vertico-posframe-get-size
        vertico-posframe-border-width 3
        vertico-posframe-parameters '((left-fringe . 10) (right-fringe . 10)))
  ;; positioning
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-truncate-lines t)


  ;; fallback-mode is used when posframe is not available,
  ;; i.e. emacs running in terminal, i.e. no window-system.
  (setq-hook! 'vertico-posframe-mode-hook
    vertico-posframe-fallback-mode 'vertico-buffer-mode))

(after! vertico-posframe
  ;; Vertico Extensions: https://github.com/minad/vertico/tree/main#extensions
  (add-hook! 'vertico-mode-hook
             #'vertico-indexed-mode
             (defun +vertico-multiform-mode-h ()
               ;; sanity check since vertico-posframe-mode .
               ;; see: https://github.com/tumashu/vertico-posframe#how-to-let-vertico-posframe-work-well-with-vertico-multiform
               (vertico-posframe-mode -1)
               (vertico-multiform-mode 1)))

  ;; more santiy checking
  (remove-hook! 'vertico-mode-hook #'vertico-posframe-mode)

  ;; Configure the display per command called.
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-git-grep buffer)
          (consult-grep buffer)
          (consult-bookmark buffer)
          (consult-recent-file buffer)
          (consult-lsp-symbols buffer)
          (consult-lsp-file-symbols buffer)
          (+default/search-project buffer)
          (+default/search-other-project buffer)
          (+default/search-project-for-symbol-at-point buffer)
          (+default/search-cwd buffer)
          (+default/search-other-cwd buffer)
          (+default/search-notes-for-symbol-at-point buffer)
          (+default/search-emacsd buffer)
          (projectile-find-file posframe
                                (vertico-posframe-border-width . 4))
          (consult-line posframe
                        (vertico-posframe-border-width . 4))
          (consult-imenu reverse buffer)
          (t posframe)))

  ;; Configure the display per completion category.
  ;; NOTE: These category settings have lower precedence than command settings.
  (setq vertico-multiform-categories
        '((imenu (:not indexed mouse))
          (symbol (vertico-sort-function . vertico-sort-alpha))))

  (after! savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  ;; These are defaults (here because you will forget)
  (map! :map vertico-map
        [M-V] #'vertico-multiform-vertical
        [M-G] #'vertico-multiform-grid
        [M-F] #'vertico-multiform-flat
        [M-R] #'vertico-multiform-reverse
        [M-U] #'vertico-multiform-unobtrusive

        [prior] #'vertico-scroll-up
        [next]  #'vertico-scroll-down
        [C-n]   #'vertico-next
        [C-p]   #'vertico-previous))

;; https://github.com/doomemacs/doomemacs/blob/5155f4aa7880c265ff8ca0c2cde011d749e57223/modules/completion/vertico/config.el#L322
(after! posframe (add-hook 'doom-after-reload-hook #'posframe-delete-all))
