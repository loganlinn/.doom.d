;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Logan Linn"
      user-mail-address "logan@loganlinn.com")

(setq doom-theme 'doom-one
      doom-themes-treemacs-theme "doom-colors"
      ;; doom-font (font-spec :family "monospace" :size 14) ;; fc-match monospace
      ;; doom-font (font-spec :family "JetBrainsMono" :size 14 :weight 'light)
      doom-font (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14)
      ;; doom-font (font-spec :family "Victor Mono" :weight 'normal :size 14)
      ;; doom-font (font-spec :family "Fira Code" :size 14 :weight 'light)
      ;; doom-font (font-spec :family "Iosevka" :weight 'light :size 14)
      ;; doom-variable-pitch-font (font-spec :family "FiraSans")
      ;; doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
      )

(setq org-directory "~/Sync/notes")

(setq fill-column 99)

(setq display-line-numbers-type 'relative)

(setq delete-by-moving-to-trash t)

(setq fancy-splash-image (concat doom-private-dir "splash.png"))

(after! pixel-scroll
  (pixel-scroll-precision-mode t)
  (setq pixel-scroll-precision-use-momentum t))

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(map! :leader
      :prefix-map ("b" . "buffer")
      :desc "Kill buffer*" "k" #'doom/kill-this-buffer-in-all-windows)
;;
(after! centaur-tabs
  (centaur-tabs-projectile-buffer-groups))

;; Disable some of ligatures enabled by (ligatures +extra)
(when (modulep! :ui ligatures +extra)
 (let ((ligatures-to-disable '(:true :false :int :float :str :bool :list :and :or :for)))
   (dolist (sym ligatures-to-disable)
     (plist-put! +ligatures-extra-symbols sym nil))))

(when (modulep! :tools lookup)
  (add-to-list '+lookup-provider-url-alist '("grep.app" "https://grep.app/search?q=%s"))
  (setq +lookup-provider-url-alist (assoc-delete-all "Google images" +lookup-provider-url-alist))
  (setq +lookup-provider-url-alist (assoc-delete-all "Google maps" +lookup-provider-url-alist))
  (setq +lookup-provider-url-alist (assoc-delete-all "DuckDuckGo" +lookup-provider-url-alist)))

(when (modulep! :completion vertico +childframe)
 (after! vertico-posframe
   vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
   vertico-posframe-truncate-lines t
   vertico-posframe-width 150
   vertico-posframe-min-height 1
   vertico-posframe-border-width 1))

(after! which-key
  (setq which-key-idle-delay 0.4))

(after! projectile
  (setq projectile-create-missing-test-files t
        projectile-project-search-path '(("~/src" . 3))
        projectile-enable-caching nil
        projectile-indexing-method 'alien)

  (defun projectile-root-poly-workspace-dir (dir)
    "Identify a project root in DIR by top-downsearch for Polylith workspace.edn in dir.
Return the first (topmost) matched directory or nil if not found."
    (locate-dominating-file dir "workspace.edn"))

  (setq projectile-project-root-functions '(projectile-root-local
                                            projectile-root-poly-workspace-dir
                                            projectile-root-bottom-up
                                            projectile-root-top-down
                                            projectile-root-top-down-recurring))


  (projectile-update-project-type 'clojure-cli
                                  :src-dir "src"
                                  :test-dir "test")

  ;; vim-projectionist (a.vim) style commands for impl<>test files
  ;; (evil-ex-define-cmd "A"  'projectile-toggle-between-implementation-and-test)
  ;; (evil-ex-define-cmd "AV" '(lambda ()
  ;;                             (interactive)
  ;;                             (evil-window-vsplit)
  ;;                             (windmove-right)
  ;;                             (projectile-toggle-between-implementation-and-test)))
  ;; (evil-ex-define-cmd "AS" '(lambda ()
  ;;                             (interactive)
  ;;                             (evil-window-split)
  ;;                             (windmove-down)
  ;;                             (projectile-toggle-between-implementation-and-test)))
  (map! :leader
        :prefix-map ("p" . "project")
        :desc "Toggle impl/test" "A" #'projectile-toggle-between-implementation-and-test))

(setq lsp-file-watch-threshold 2000)

(after! evil
  ;; Focus new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t)

  ;; thicc finger support
  (evil-ex-define-cmd "W" #'evil-write)
  (evil-ex-define-cmd "E" #'evil-edit)
  (evil-ex-define-cmd "Sort" #'evil-edit)

  (map! :map prog-mode-map ; unwanted in vterm-mode...
        :nv "C-a" #'evil-numbers/inc-at-pt
        :nv "C-S-a" #'evil-numbers/dec-at-pt))

(use-package! evil-cleverparens
  :hook (clojure-mode . evil-cleverparens-mode)
  :init
  ;; Fix evil-cleverparens in terminal (https://github.com/emacs-evil/evil-cleverparens/issues/58)
  ;; 1. disable additional bindings so they aren't bound when the package loads
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  ;; 2. turn on the "additional-bindings" so that when we call `evil-cp-set-additional-bindings` it will bind keys
  (setq evil-cleverparens-use-additional-bindings t)
  (unless window-system
    ;; 3 when we're in the terminal, delete the bindings for M-[ and M-] from the alist of additional bindings
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings))
    (setq evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings)))
  ;; 4. bind all the keys listed in evil-cp-additional-bindings
  (evil-cp-set-additional-bindings))

(after! expand-region
  (map! :nvi
        :desc "Expand region"
        "M-=" #'er/expand-region
        :desc "Reverse expand region"
        "M--" (lambda () (interactive) (er/expand-region -1))))

(add-hook! (go-mode rust-mode sh-mode) #'format-all-mode)

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (treemacs-project-follow-mode +1)
  (map! :map treemacs-mode-map
        :desc "Expand" [mouse-1] #'treemacs-single-click-expand-action
        :desc "Rename file" [f2] #'treemacs-rename-file
        :desc "Refresh" [f5] #'treemacs-refresh))

(after! flycheck
  (setq flycheck-navigation-minimum-level 'error)
  ;; error navigation a la IntelliJ
  (map!
   :desc "Jump to next error" [f2]   #'flycheck-next-error
   :desc "Jump to prev error" [S-f2] #'flycheck-previous-error))

(after! gist
  (setq gist-view-gist t))

(after! lsp
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
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"))
    (push dir lsp-file-watch-ignored-directories))

  (map! :desc "Rename" [S-f6] #'lsp-rename))

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
   (lambda ()
     (interactive)
     (browse-url (concat "https://linear.app/patch-tech/issue/"
                         (buffer-substring
                          (previous-single-property-change (point) 'mouse-face)
                          (next-single-property-change (point) 'mouse-face)))))
   :face             'link
   :face-policy      'prepend
   :keyboard-binding "RET"))

;;; :lang

(use-package! protobuf-mode :defer t)

(use-package! yuck-mode :defer t)

(after! org
  (add-to-list 'org-modules 'ol-man))

(add-hook! 'sh-mode-hook
  (if (string-match "\\.zsh$" buffer-file-name)
      (sh-set-shell "zsh")))

;;; :lang v
;; (use-package! v-mode
;;   :defer t
;;   :config
;;   (map! :localleader
;;         :map v-mode-map
;;         "m" #'v-menu
;;         "f" #'v-format-buffer))

(when (modulep! :tools magit)
  (load! "+magit"))

(when (modulep! :lang clojure)
  (load! "+clojure"))

(when (modulep! :lang crystal)
  (load! "+crystal"))

(when (modulep! :lang nix)
  (load! "+nix"))

(when (modulep! :lang emacs-lisp)
  (load! "+emacs-lisp"))

(defun load-dir-local-variables ()
  "Apply directory-local variables."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun chmod-this-file (&optional path mode)
  "Set executable mode bit of current file"
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         (read-file-modes "File modes (octal or symbolic): "
                          (buffer-file-name (buffer-base-buffer)))))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer)))))
    (unless (file-exists-p path)
     (user-error "Buffer is not visiting any file"))
   (chmod path mode)))

(map! :leader
      :prefix-map ("f" . "file")
      :desc "Change file mode bits" "M" #'chmod-this-file)

;; Load per-system config
(load! (concat "+systems/" (system-name)) (dir!) t)
