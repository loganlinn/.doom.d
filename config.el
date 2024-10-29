;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(add-hook! 'doom-after-init-hook
  (defun doom--end-init-h ()
    "Set `doom-init-time'."
    (when (doom-context-pop 'init)
      (setq doom-init-time (float-time (time-subtract (current-time) before-init-time))))))

(setq user-full-name "Logan Linn"
      user-mail-address "logan@loganlinn.com"
      fill-column 99
      display-line-numbers-type 'relative
      fancy-splash-image (concat doom-private-dir "splash.png"))

;; macos: ⌘ instead of ⌥ for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq delete-by-moving-to-trash t
      trash-directory (cond (IS-MAC "") ))

(setq doom-theme 'doom-one
      doom-one-padded-modeline nil
      doom-one-brighter-modeline t
      doom-one-brighter-comments nil)

(setq doom-font (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :weight 'normal :size 14)
      doom-themes-treemacs-theme "doom-colors"
      doom-themes-treemacs-enable-variable-pitch nil)

(when (modulep! :ui doom-dashboard)
  (add-to-list '+doom-dashboard-menu-sections
               '("Open daily note"
                 :icon (nerd-icons-faicon "nf-fa-calendar" :face 'doom-dashboard-menu-title)
                 :when (featurep! :lang org +journal)
                 :face (:inherit (doom-dashboard-menu-title bold))
                 :action org-roam-dailies-goto-today)))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t)
  (setq-hook! 'pixel-scroll-mode
    pixel-scroll-precision-use-momentum t))

(setq +lookup-provider-url-alist
      (append '(("github :: code" "https://github.com/search?q=%s&type=code")
                ("github :: stars" "https://github.com/loganlinn?tab=stars&q=%s"))
              (mapcar (fn! (list (concat "grep.app" (when % (concat " :: " %)))
                                 (concat "https://grep.app/search?q=%s"
                                         (when % (concat "&filter[lang][0]=" (url-encode-url %))))))
                      '(nil "Clojure" "Emacs Lisp" "Go" "Lua" "Nix" "Python" "Rust" "Shell"))
              '(("mvn" "https://mvnrepository.com/search?q=%s")
                ("gmail" "https://mail.google.com/mail/u/0/#search/%s")
                ("fastmail" "https://app.fastmail.com/mail/search:%s"))))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Kill buffer everywhere"     "k" #'doom/kill-this-buffer-in-all-windows
       :desc "Kill matching buffers"      "/" #'doom/kill-matching-buffers)

      (:prefix-map ("p" . "project")
       :desc "Kill other project buffers" "K" #'doom/kill-project-buffers)

      (:prefix-map ("f" . "file")
       :desc "chmod"                      "M" #'chmod-this-file
       :desc "ranger"                     "g" #'ranger)

      (:prefix-map ("q" . "quit/session")
       :desc "Quit Emacs without saving"  "!" #'evil-quit-all-with-error-code)

      (:prefix-map ("c" . "code")
       :when (modulep! :completion vertico)
       :when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
       :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-file-symbols ;; default is #'consult-lsp-symbols
       :desc "Jump to symbol in any workspace"     "J"   #'consult-lsp-symbols)

      (:prefix-map ("g" . "git")
       (:when (modulep! :tools magit)
         ;; don't ask for file; use current (magit-stage-file behavior changed?)
         :desc "Git stage diff"            "w"   #'magit-diff-staged
         :desc "Git stage file"            "S"   (cmd!! #'magit-stage-buffer-file #'magit-staged-files)
         :desc "Git unstage file"          "U"   (cmd!! #'magit-unstage-buffer-file #'magit-staged-files)
         (:prefix ("o" . "open in browser")
          :desc "Browse assigned requests" "a" (cmd! (browse-url "https://github.com/pulls/assigned"))
          :desc "Browse review requests"   "R" (cmd! (browse-url "https://github.com/pulls/review-requested"))
          :desc "Graphite dashboard"       "g" (cmd! (browse-url "https://app.graphite.dev/")))))

      (:when (modulep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Swap left"  "<" #'+workspace/swap-left
        :desc "Swap right" ">" #'+workspace/swap-right)))

(map! [mouse-8] #'switch-to-prev-buffer
      [mouse-9] #'switch-to-next-buffer
      [f12]     #'+lookup/definition
      [S-f12]   #'+lookup/references
      [C-f12]   #'+lookup/implementations

      (:after expand-region
       :nvi :desc "Expand region" "M-=" #'er/expand-region
       :nvi :desc "Reverse expand region" "M--" (cmd! (er/expand-region -1)))

      (:after evil
       :map prog-mode-map ; unwanted in term modes
       :nv "C-a" #'evil-numbers/inc-at-pt
       :nv "C-S-a" #'evil-numbers/dec-at-pt)

      (:after evil
       :map view-mode-map ; i.e. read-only files
       :n "0" #'evil-beginning-of-line)

      (:after company
       :map company-active-map
       [prior] #'company-previous-page
       [next] #'company-page-next)

      (:after vertico
       :map vertico-map
       :desc "Quick insert" [M-q] #'vertico-quick-insert
       :desc "Quick exit" [C-q] #'vertico-quick-exit
       ;; vertico-exit completes with first suggestion, while
       ;; vertico-exit-input completes with current input.
       ;; This distinction is needed when, for example, an existing file is being renamed.
       :desc "Exit with input" [M-return] #'vertico-exit-input)

      (:after projectile
       :leader
       :prefix-map ("p" . "project")
       :desc "Toggle impl/test" "A" #'projectile-toggle-between-implementation-and-test)

      (:after treemacs
       :map treemacs-mode-map
       :desc "Expand" [mouse-1] #'treemacs-single-click-expand-action
       :desc "Rename file" [f2] #'treemacs-rename-file
       :desc "Refresh" [f5] #'treemacs-refresh)

      (:after flycheck
       :map flycheck-mode-map
       :desc "Jump to next error" [f2]   #'flycheck-next-error
       :desc "Jump to prev error" [S-f2] #'flycheck-previous-error)

      (:after lsp-mode
       :map lsp-mode-map
       ;; :desc "Find definition" [f12] #'lsp-find-definition
       :desc "Find references" [S-f12] #'lsp-find-references
       :desc "Find implementations" [C-f12] #'lsp-find-implementation
       :desc "Code actions..." [M-return] #'lsp-execute-code-action
       :desc "Rename" [S-f6] #'lsp-rename))

(set-popup-rules!
  ;;  `help-mode', `helpful-mode' (same as default, excl :quit)
  '(("^\\*\\([Hh]elp\\|Apropos\\)" :slot 2 :vslot -8 :size 0.42 :select t :quit 'current)
    ("^\\*Async Shell Command" :vslot -5 :select t :quit t :ttl 10)))

;;; packages

(use-package! evil-cleverparens
  :after evil smartparens
  :init
  ;; Fix evil-cleverparens in terminal (https://github.com/emacs-evil/evil-cleverparens/issues/58)
  (setq evil-cleverparens-use-additional-bindings nil)
  :config
  ;; When using terminal emacs, remap M-[ and M-], to M-b and M-B, respectively
  (unless window-system
    (setq evil-cp-additional-bindings (assoc-delete-all "M-[" evil-cp-additional-bindings))
    (setq evil-cp-additional-bindings (assoc-delete-all "M-]" evil-cp-additional-bindings))
    (add-to-list 'evil-cp-additional-bindings '("M-b" . evil-cp-wrap-next-square))
    (add-to-list 'evil-cp-additional-bindings '("M-B" . evil-cp-wrap-previous-square)))

  (evil-set-command-properties 'evil-cp-change :move-point t)
  ;; enable these text-objects globally
  (require 'evil-cleverparens-text-objects)
  (add-hook! 'evil-cleverparens-mode-hook
    (evil-cp-set-additional-movement-keys)
    (evil-cp-set-additional-bindings))

  (setq evil-cleverparens-use-regular-insert nil
        evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-move-skip-delimiters nil
        evil-cleverparens-complete-parens-in-yanked-region nil
        evil-want-fine-undo t
        evil-move-beyond-eol t)

  ;; (map! :map evil-cleverparens-map
  ;;       :leader
  ;;       "(" #'evil-cp-wrap-next-round
  ;;       ")" #'evil-cp-wrap-previous-round)
  )

(use-package! highlight-parenthesis
  :defer t)

(use-package! fence-edit
  :commands fence-edit-code-region fence-edit-code-region-with-mode fence-edit-code-at-point fence-edit-dwim)

(after! which-key
  (setq which-key-idle-delay 0.4))

(after! treemacs
  (treemacs-project-follow-mode +1))

(after! projectile
  (setq projectile-create-missing-test-files t
        projectile-project-search-path '(("~/src" . 3))
        projectile-sort-order 'recently-active ;; recently active buffers and recently opened files
        projectile-current-project-on-switch 'move-to-end
        projectile-enable-caching nil
        ;; projectile-indexing-method 'hybrid ;; disabled due issues with files that exist not being reported
        projectile-indexing-method 'alien))

(after! (:and projectile treemacs)
  (add-hook! 'projectile-before-switch-project-hook
    (defun +treemacs--show-after-switch-project ()
      (require 'treemacs)
      (save-current-buffer
       (pcase (treemacs-current-visibility)
         ('exists  (treemacs-select-window))
         ('none    (treemacs--init)))))))

(after! (:and projectile centaur-tabs)
  (centaur-tabs-projectile-buffer-groups))

(after! forge
  (setq  forge-topic-list-limit '(100 . -10)
         forge-owned-accounts '(("loganlinn"
                                 "patch-tech"
                                 "plumatic"
                                 "omcljs"))))

;; TODO configure this for Linear ticket references
;; (use-package! bug-reference
;;   :defer t
;;   :config
;;   (defun bug-reference--url-handler ()
;;     (print! "bug-reference-bug-regexp matched %s, %s" (match-string 1) (match-string 2))
;;     nil)
;;  (setq-local bug-reference-bug-regexp "\\(\\b\\(?:[Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z+-]+/\\|PAT-\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)"
;;        bug-reference-url-format #'bug-reference--url-handler))

;;; :editor

(after! evil
  ;; Focus new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t)

  ;; thicc finger support
  (evil-ex-define-cmd "W" #'evil-write)
  (evil-ex-define-cmd "E" #'evil-edit)
  (evil-ex-define-cmd "Sort" #'evil-edit)

  ;; https://github.com/tpope/vim-fugitive
  (when (modulep! :emacs vc)
   (evil-ex-define-cmd "Gre ad" #'vc-revert)
   (evil-ex-define-cmd "Gdiff" #'vc-diff)
   (evil-ex-define-cmd "Ggrep`'" #'vc-git-grep)
   (evil-ex-define-cmd "GMove" #'vc-rename-file)
   (evil-ex-define-cmd "GDelete" #'vc-delete-file)
   (evil-ex-define-cmd "GBrowse" #'+vc/browse-at-remote))

  (when (modulep! :tools magit)
    (evil-ex-define-cmd "Gwrite" #'magit-stage-file)))

(after! format-all
  (add-hook! 'go-mode-hook #'format-all-mode)
  (add-hook! 'rust-mode-hook #'format-all-mode)
  (add-hook! 'sh-mode-hook #'format-all-mode))

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

(use-package! aggressive-indent
  :commands aggressive-indent-mode aggressive-indent-global-mode
  :config
  ;; (setq aggressive-indent-region-function #'+format/region)
  (setq aggressive-indent-region-function #'indent-region))

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

(use-package! crux
  :commands
  crux-open-with
  crux-smart-kill-line
  crux-smart-open-line-above
  crux-smart-open-line
  crux-cleanup-buffer-or-region
  crux-recentf-find-file
  crux-recentf-find-directory
  crux-view-url
  crux-eval-and-replace
  crux-transpose-windows
  crux-delete-file-and-buffer
  crux-copy-file-preserve-attributes
  crux-duplicate-current-line-or-region
  crux-duplicate-and-comment-current-line-or-region
  crux-rename-file-and-buffer
  crux-visit-term-buffer
  crux-kill-other-buffers
  crux-indent-defun
  crux-indent-rigidly-and-copy-to-clipboard
  crux-find-user-init-file
  crux-find-user-custom-file
  crux-find-shell-init-file
  crux-top-join-line
  crux-kill-whole-line
  crux-kill-line-backwards
  crux-kill-and-join-forward
  crux-kill-buffer-truename
  crux-ispell-word-then-abbrev
  crux-upcase-region
  crux-downcase-region
  crux-capitalize-region
  crux-other-window-or-switch-buffer)

(use-package! keycast
  :commands keycast-mode)

(use-package! prisma-mode
  :defer t)

(use-package! protobuf-mode
  :defer t)

(use-package! yuck-mode
  :defer t)

;; (use-package! minions
;;   :defer t
;;   :config
;;   (map! "<S-down-mouse-3>" #'minions-minor-modes-menu))

;;; :lang

(add-hook! 'sh-mode-hook
  (when (string-match "\\.zsh$" buffer-file-name)
    (sh-set-shell "zsh")))

(defun my/turn-on-lisp-modes ()
  (interactive)
  (rainbow-delimiters-mode 1)
  ;; does not respect :style/indent metadata...
  ;; (aggressive-indent-mode 1)
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

(add-hook! ('clojure-mode-hook
            'cider-repl-mode-hook
            'common-lisp-mode-hook
            'emacs-lisp-mode-hook
            'eval-expression-minibuffer-setup-hook
            'lisp-data-mode-hook
            'eshell-mode-hook
            'fennel-mode-hook
            'fennel-repl-mode-hook
            'geiser-repl-mode-hook
            'gerbil-mode-hook
            'inferior-emacs-lisp-mode-hook ;; ielm
            'inferior-lisp-mode-hook
            'inferior-scheme-mode-hook
            'lisp-data-mode-hook
            'lisp-interaction-mode-hook
            'lisp-mode-hook
            'scheme-mode-hook
            'yuck-mode-hook
            'janet-mode-hook)
           :append #'my/turn-on-lisp-modes)

(use-package! powershell :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "+clojure")
(load! "+crystal")
(load! "+emacs-lisp")
(load! "+javascript")
(load! "+kubernetes")
(load! "+lsp")
(load! "+nix")
(load! "+org")
(load! "+vertico")
(when IS-MAC (load! "+darwin"  nil t))
(when IS-LINUX (load! "+linux"  nil t))
(when IS-WINDOWS (load! "+windows"  nil t))
(load! (concat "+systems/" (system-name)) nil t)
(load! "+local" nil t)
