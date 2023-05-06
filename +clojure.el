;;; :lang clojure
(use-package! clojure-mode
  :config
  (setq clojure-toplevel-inside-comment-form t)

  (map! :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        (:localleader
         (:when (modulep! :tools lsp)
           :desc "Clean ns" "o" #'lsp-clojure-clean-ns))))

(use-package! cider
  :after clojure-mode
  :config
  (setq cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-print-fn 'puget
        cider-repl-history-size 1000
        cider-known-endpoints nil
        ;;cider-repl-buffer-size-limit 200
        cider-enrich-classpath t)

  (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.36.0")
  (cider-add-to-alist 'cider-jack-in-dependencies "io.github.nextjournal/clerk" "0.13.842")
  (cider-add-to-alist 'cider-jack-in-dependencies "philoskim/debux" "0.8.2")
  (cider-add-to-alist 'cider-jack-in-dependencies "com.clojure-goes-fast/clj-java-decompiler" "0.3.3")
  (cider-add-to-alist 'cider-jack-in-dependencies "criterium" "0.4.6")
  (cider-add-to-alist 'cider-jack-in-dependencies "prismatic/plumbing" "0.6.0")

  (defun +cider-jack-in-clj-polylith (params)
    "Start an nREPL server for the current Polylith workspace and connect to it."
    (interactive "P")
    (let ((ws-dir (locate-dominating-file (pwd) "workspace.edn")))
      (if ws-dir
          (progn
            (message "Starting nREPL server from `%s'" ws-dir)
            (cider-jack-in-clj (plist-put params :project-dir ws-dir)))
        (error "Unable to locate 'workspace.edn' in current directory or parent directory"))))


  (defun portal/open ()
    (interactive)
    (cider-interactive-eval
     "(do
        (in-ns 'user)
        (require 'portal.api)
        (defonce portal nil)
        (defonce portal-shutdown-hook-added
          (do (.addShutdownHook (Runtime/getRuntime) (Thread. portal.api/close)) true))
        (alter-var-root #'user/portal portal.api/open)
        (add-tap #'portal.api/submit)
        (portal.api/url portal))"))

  (defun portal/clear ()
    (interactive)
    (cider-interactive-eval
     "(if-let [var (requiring-resolve 'user/portal)]
        (do ((requiring-resolve 'portal.api/clear) @var)
            \"Cleared\")
        \"No session found.\")"))

  (defun portal/close ()
    (interactive)
    (cider-interactive-eval
     "(when-let [var (requiring-resolve 'user/portal)]
        (when (some? @var)
          (alter-var-root var (requiring-resolve 'portal.api/close))
          (remove-tap (requiring-resolve 'portal.api/submit))
          \"Closed\"))"))

  (defun portal/docs ()
    (interactive)
    (cider-interactive-eval
     "((requiring-resolve 'portal.api/docs))"))

  (require 'easymenu)
  (easy-menu-define portal-menu clojure-mode-map
    "Menu for Portal (Clojure data navigator)"
    '("Portal"
      ["Open" portal/open]
      ["Clear" portal/clear]
      ["Close" portal/close]
      "-"
      ["Docs" portal/docs]))

  (map! (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
         :desc "Portal: Open"  [f8]     #'portal/open
         :desc "Portal: Clear" [C-f8]   #'portal/clear
         :desc "Portal: Close" [S-f8]   #'portal/close
         :desc "Portal: Docs"  [C-S-f8] #'portal/docs))

  ;;;

  (defun clerk/serve ()
    (interactive)
    (cider-interactive-eval
     "((requiring-resolve 'nextjournal.clerk/serve!) {:browse? true})"))

  (defun clerk/show ()
    (interactive)
    (when-let ((filename (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "((requiring-resolve 'nextjournal.clerk/show!) \"" filename "\")"))))

  (map! :map cider-mode-map
        "C-c M-k" #'+cider-jack-in-clj-polylith)

  (map! :map cider-repl-mode-map
        :ni "C-p" #'cider-repl-backward-input))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (map! (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
         :ni [M-return] #'cljr-add-missing-libspec)
        (:map clojure-refactor-map
         :desc "Clean ns" "o"  #'lsp-clojure-clean-ns
         :desc "Clean ns" "C-o"  #'lsp-clojure-clean-ns
         ;; older
         :desc "Add missing libspec" "n a" #'cljr-add-missing-libspec
         :desc "Clean ns" "n c" #'lsp-clojure-clean-ns))

  (setq cljr-add-ns-to-blank-clj-files t
        cljr-insert-newline-after-require t
        cljr-print-miser-width 40
        cljr-print-right-margin 80
        ;; cljr-project-clean-functions '(lsp-clojure-clean-ns)
        cljr-slash-uses-suggest-libspec t
        cljr-hotload-dependencies nil
        cljr-magic-require-namespaces
        '(;; Clojure
          ("as"    . "clojure.core.async")
          ("csv"   . "clojure.data.csv")
          ("edn"   . "clojure.edn")
          ("io"    . "clojure.java.io")
          ("log"   . "clojure.tools.logging")
          ("mat"   . "clojure.core.matrix")
          ("nrepl" . "clojure.nrepl")
          ("pp"    . "clojure.pprint")
          ("s"     . "clojure.spec.alpha")
          ("set"   . "clojure.set")
          ("sh"    . "clojure.java.shell")
          ("spec"  . "clojure.spec.alpha")
          ("str"   . "clojure.string")
          ("walk"  . "clojure.walk")
          ("xml"   . "clojure.data.xml")
          ("zip"   . "clojure.zip")
          ;; Others
          ("http"     . "clj-http.client")
          ("json"     . "cheshire.core")
          ("m"        . "malli.core")
          ("p"        . "plumbing.core")
          ("sql"      . "honey.sql")
          ("sqlh"     . "honey.sql.helpers")
          ("jt"       . "java-time")
          ("yaml"     . "clj-yaml.core"))))

  (use-package! evil-cleverparens
    :after evil
    :init
    (setq evil-cleverparens-use-regular-insert nil
          evil-cleverparens-swap-move-by-word-and-symbol t
          evil-cleverparens-move-skip-delimiters nil
          evil-want-fine-undo t
          evil-move-beyond-eol t)
    :config
    (evil-set-command-properties 'evil-cp-change :move-point t)

    (add-hook! 'emacs-lisp-mode-hook
      (evil-cleverparens-mode t)
      (smartparens-strict-mode t))

    (add-hook! 'clojure-mode-hook
      (evil-cleverparens-mode t)
      (smartparens-strict-mode t))

    (add-hook! 'cider-repl-mode-hook
      (evil-cleverparens-mode t)
      (smartparens-strict-mode t)))

(use-package! sesman
  :after cider
  :config
  (defun link-cider-session ()
    "Link the current buffer to a running CIDER session."
    (interactive)
    (setq sesman-system 'CIDER)
    (sesman-link-with-buffer)))
