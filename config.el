;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Logan Linn"
      user-mail-address "logan@loganlinn.com"
      fill-column 99
      display-line-numbers-type 'relative
      delete-by-moving-to-trash t
      fancy-splash-image (concat doom-private-dir "splash.png"))

(setq doom-theme 'doom-one
      doom-one-padded-modeline nil
      doom-one-brighter-modeline t
      doom-one-brighter-comments nil)

(setq doom-font (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14)
      ;; (font-spec :family "monospace" :size 14) ;; fc-match monospace
      ;; (font-spec :family "JetBrainsMono" :size 14 :weight 'light)
      ;; (font-spec :family "Victor Mono" :weight 'normal :size 14)
      ;; (font-spec :family "Fira Code" :size 14 :weight 'light)
      ;; (font-spec :family "Iosevka" :weight 'light :size 14)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14)
      ;; (font-spec :family "monospace")
      ;; (font-spec :family "Noto Sans" :size 13)
      ;; (font-spec :family "FiraSans")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      ;; doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-themes-treemacs-theme "doom-colors"
      doom-themes-treemacs-enable-variable-pitch nil
      doom-scratch-initial-major-mode t)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(map!
 [mouse-8] #'previous-buffer
 [mouse-9] #'next-buffer
 (:leader
  (:prefix-map ("b" . "buffer")
   ;; kill more aggressively
   :desc "Kill buffer everywhere"       "k" #'doom/kill-this-buffer-in-all-windows
   :desc "Kill matching buffers"        "/" #'doom/kill-matching-buffers)
  (:prefix-map ("p" . "project")
   :desc "Kill other project buffers"    "K" #'doom/kill-project-buffers)
  (:prefix-map ("f" . "file")
   :desc "Change file mode bits" "M" #'chmod-this-file
   :desc "Ranger" "g" #'ranger)
  (:prefix-map ("q" . "quit/session")
   :desc "Quit Emacs without saving"    "!" #'evil-quit-all-with-error-code))

 (:after treemacs
  :map treemacs-mode-map
  :desc "Expand" [mouse-1] #'treemacs-single-click-expand-action
  :desc "Rename file" [f2] #'treemacs-rename-file
  :desc "Refresh" [f5] #'treemacs-refresh)

 (:after expand-region
  :nvi :desc "Expand region" "M-=" #'er/expand-region
  :nvi :desc "Reverse expand region" "M--" (cmd! (er/expand-region -1)))

 (:after evil
  :map prog-mode-map ; unwanted in term modes
  :nv "C-a" #'evil-numbers/inc-at-pt
  :nv "C-S-a" #'evil-numbers/dec-at-pt)

 (:after lsp-mode
  :ni :desc "Code actions..." [M-return] #'lsp-execute-code-action
  :nvi :desc "Rename" [S-f6] #'lsp-rename)

 (:after flycheck
  :map flycheck-mode-map
  :desc "Jump to next error" [f2]   #'flycheck-next-error
  :desc "Jump to prev error" [S-f2] #'flycheck-previous-error)

 (:after projectile
  :leader
  :prefix-map ("p" . "project")
  :desc "Toggle impl/test" "A" #'projectile-toggle-between-implementation-and-test)

 (:after vertico
  :map vertico-map
  :desc "Quick insert" [M-q] #'vertico-quick-insert
  :desc "Quick exit" [C-q] #'vertico-quick-exit
  ;; vertico-exit completes with first suggestion, while
  ;; vertico-exit-input completes with current input.
  ;; This distinction is needed when, for example, an existing file is being renamed.
  :desc "Exit with input" [M-return] #'vertico-exit-input)

 (:after company
  :map company-active-map
  [prior] #'company-previous-page
  [next] #'company-page-next))

;;; :ui

(use-package! alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(after! projectile
  (setq projectile-create-missing-test-files t
        projectile-project-search-path '(("~/src" . 3))
        projectile-sort-order 'recentf
        projectile-current-project-on-switch 'move-to-end
        projectile-enable-caching nil
        ;; projectile-indexing-method 'hybrid ;; disabled due issues with files that exist not being reported
        projectile-indexing-method 'alien)

  (add-to-list 'projectile-other-file-alist '("clj" . ("cljc" "cljs")))
  (add-to-list 'projectile-other-file-alist '("cljs" . ("cljc" "clj")))
  (add-to-list 'projectile-other-file-alist '("cljc" . ("clj" "cljs")))
  (add-to-list 'projectile-other-file-alist '("edn" . ("clj"))))

(use-package! vertico-posframe
  :after-call vertico
  :config
  (add-hook 'doom-after-reload-hook #'posframe-delete-all))

(after! vertico
  (vertico-indexed-mode 1)
  ;; vertico-posframe + vertico
  ;; Temporary toggling between the different display modes is possible.
  ;; The following keys are bound in the `vertico-multiform-map'.
  ;;
  ;;   M-V -> `vertico-multiform-vertical'
  ;;   M-G -> `vertico-multiform-grid'
  ;;   M-F -> `vertico-multiform-flat'
  ;;   M-R -> `vertico-multiform-reverse'
  ;;   M-U -> `vertico-multiform-unobtrusive'

  ;; NOTE: vertico-posframe-mode will be activated/deactivated by vertico-multiform-mode
  ;; dynamically when you add ‘posframe’ setting to vertico-multiform-commands,
  ;; please *do not enable vertico-posframe-mode globally at the moment.*
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-git-grep buffer)
          (consult-grep buffer)
          (consult-bookmark buffer)
          (consult-recent-file buffer)
          (+default/search-project buffer)
          (+default/search-other-project buffer)
          (+default/search-project-for-symbol-at-point buffer)
          (+default/search-cwd buffer)
          (+default/search-other-cwd buffer)
          (+default/search-notes-for-symbol-at-point buffer)
          (+default/search-emacsd buffer)
          (projectile-find-file posframe
                                (vertico-posframe-poshandler . posframe-poshandler-frame-center)
                                (vertico-posframe-border-width . 4)
                                (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-line posframe
                        (vertico-posframe-poshandler . posframe-poshandler-frame-center)
                        (vertico-posframe-border-width . 4)
                        (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (consult-imenu reverse buffer)
          (t posframe))

        ;; Alist of categories/regexps and list of settings to turn on per category.
        ;; See `vertico-multiform-commands' on details about the settings.  The
        ;; category settings have lower precedence than
        vertico-multiform-categories
        '((imenu (:not indexed mouse))
          (symbol (vertico-sort-function . vertico-sort-alpha))))

  (setq vertico-posframe-fallback-mode 'vertico-buffer-mode
        vertico-posframe-size-function #'vertico-posframe-get-size
        vertico-posframe-truncate-lines t
        vertico-posframe-border-width 3
        vertico-posframe-parameters '((left-fringe . 10)
                                      (right-fringe . 10)))
  ;; The builtin poshandler functions:
  ;; 1.  `posframe-poshandler-frame-center'
  ;; 2.  `posframe-poshandler-frame-top-center'
  ;; 3.  `posframe-poshandler-frame-top-left-corner'
  ;; 4.  `posframe-poshandler-frame-top-right-corner'
  ;; 5.  `posframe-poshandler-frame-top-left-or-right-other-corner'
  ;; 6.  `posframe-poshandler-frame-bottom-center'
  ;; 7.  `posframe-poshandler-frame-bottom-left-corner'
  ;; 8.  `posframe-poshandler-frame-bottom-right-corner'
  ;; 9.  `posframe-poshandler-window-center'
  ;; 10.  `posframe-poshandler-window-top-center'
  ;; 11. `posframe-poshandler-window-top-left-corner'
  ;; 12. `posframe-poshandler-window-top-right-corner'
  ;; 13. `posframe-poshandler-window-bottom-center'
  ;; 14. `posframe-poshandler-window-bottom-left-corner'
  ;; 15. `posframe-poshandler-window-bottom-right-corner'
  ;; 16. `posframe-poshandler-point-top-left-corner'
  ;; 17. `posframe-poshandler-point-bottom-left-corner'
  ;; 18. `posframe-poshandler-point-bottom-left-corner-upward'
  ;; 19. `posframe-poshandler-point-window-center'
  ;; 20. `posframe-poshandler-point-frame-center'
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))

(after! (:and vertico savehist)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))


(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t)
  (setq-hook! 'pixel-scroll-mode
    pixel-scroll-precision-use-momentum t))

(after! which-key
  (setq which-key-idle-delay 0.4))

(after! treemacs
  (treemacs-project-follow-mode +1))

(after! (centaur-tabs projectile)
  (centaur-tabs-projectile-buffer-groups))

(after! forge
  (setq  forge-topic-list-limit '(100 . -10)
         forge-owned-accounts '(("loganlinn"
                                 "patch-tech"
                                 "plumatic"
                                 "omcljs"))))

;;; :editor

(after! evil
  ;; Focus new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t)

  ;; thicc finger support
  (evil-ex-define-cmd "W" #'evil-write)
  (evil-ex-define-cmd "E" #'evil-edit)
  (evil-ex-define-cmd "Sort" #'evil-edit))

(after! (term evil)
  (evil-collection-term-setup))

(after! format-all
  (add-hook! 'go-mode-hook #'format-all-mode)
  (add-hook! 'rust-mode-hook #'format-all-mode)
  (add-hook! 'sh-mode-hook #'format-all-mode))

(use-package! evil-cleverparens
  :after evil smartparens
  :init
  ;; Fix evil-cleverparens in terminal (https://github.com/emacs-evil/evil-cleverparens/issues/58)
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  (evil-set-command-properties 'evil-cp-change :move-point t)

  ;; enable these text-objects globally
  (require 'evil-cleverparens-text-objects)

  (setq evil-cleverparens-use-regular-insert nil
        evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-move-skip-delimiters nil
        evil-cleverparens-complete-parens-in-yanked-region nil
        evil-want-fine-undo t
        evil-move-beyond-eol t))

(use-package! highlight-parenthesis
  :defer t)

;; (add-hook! 'vterm-mode-hook
;;   (evil-collection-vterm-escape-stay))


;;; :checkers

(after! flycheck
  (setq flycheck-navigation-minimum-level 'error))

;;; :tools

(after! magit
  (setq magit-diff-refine-hunk 'all
        magit-repository-directories '(("~/src" . 3))
        magit-clone-default-directory #'magit--default-clone-directory-handler
        magit-save-repository-buffers nil
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        transient-values '((magit-rebase "--autosquash" "--autostash")
                           (magit-pull "--rebase" "--autostash")
                           (magit-revert "--autostash")))

  ;; automaticailly refresh magit buffers when files are saved
  (add-hook! 'after-save-hook #'magit-after-save-refresh-status))

(after! gist
  (setq gist-view-gist t))

(when (modulep! :tools lookup)
  (add-to-list '+lookup-provider-url-alist '("grep.app" "https://grep.app/search?q=%s"))
  (add-to-list '+lookup-provider-url-alist '("github" "https://github.com/search?q=%s&type=code")))

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 8264
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-lens-enable t)

  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]\\.devenv\\'"
                 "[/\\\\]\\.go\\'"
                 "[/\\\\]\\.env\\'"
                 "[/\\\\]\\.local\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
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

(use-package! button-lock
  :defer t
  :config
  (button-lock-set-button
   "PAT-[0-9]+"
   (cmd! (let ((issue (buffer-substring (previous-single-property-change (point) 'mouse-face)
                                        (next-single-property-change (point) 'mouse-face))))
          (browse-url (concat "https://linear.app/patch-tech/issue/" issue))))
   :face             'link
   :face-policy      'prepend
   :keyboard-binding "RET"))

;;; :lang

(after! org
  (setq org-directory (or (getenv "XDG_NOTES_DIR") "~/Sync/notes"))
  (add-to-list 'org-modules 'ol-man))

(use-package! protobuf-mode
  :defer t)

(use-package! yuck-mode
  :defer t)

(add-hook! 'sh-mode-hook
  (when (string-match "\\.zsh$" buffer-file-name)
    (sh-set-shell "zsh")))

(load! "+clojure")
(load! "+crystal")
(load! "+nix")
(load! "+emacs-lisp")

(defun +loganlinn/turn-on-lisp-modes ()
  (interactive)

  (rainbow-delimiters-mode 1)

  (aggressive-indent-mode 1)

  (smartparens-global-mode 1)
  (turn-on-smartparens-strict-mode)

  (setq evil-cleverparens-use-additional-bindings t)

  ;; When using terminal emacs, remap M-[ and M-], to M-b and M-B, respectively
  (unless window-system
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings))
    (setq evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings))
    (add-to-list 'evil-cp-additional-bindings '("M-b" . evil-cp-wrap-next-square))
    (add-to-list 'evil-cp-additional-bindings '("M-B" . evil-cp-wrap-previous-square)))
  (evil-cp-set-additional-movement-keys)
  (evil-cp-set-additional-bindings)
  (evil-cleverparens-mode 1))

(add-hook! 'clojure-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'clojurec-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'clojurescript-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'cider-repl-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'common-lisp-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'emacs-lisp-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'lisp-data-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'eshell-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'fennel-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'fennel-repl-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'geiser-repl-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'gerbil-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'inferior-emacs-lisp-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'inferior-lisp-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'inferior-scheme-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'lisp-data-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'lisp-interaction-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'lisp-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'scheme-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'yuck-mode-hook #'+loganlinn/turn-on-lisp-modes)
(add-hook! 'janet-mode-hook #'+loganlinn/turn-on-lisp-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! (concat "+systems/" (system-name)) nil t)
(load! "+local" nil t)
