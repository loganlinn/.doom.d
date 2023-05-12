
(add-hook! clojure-mode
  (setq clojure-toplevel-inside-comment-form t)

  (rainbow-delimiters-mode +1)
  (aggressive-indent-mode +1)

  (map! :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        (:localleader
         (:when (modulep! :tools lsp)
           :desc "Clean ns" "o" #'lsp-clojure-clean-ns))))

(add-hook! cider-mode
  (setq cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-print-fn 'puget
        cider-repl-history-size 1000
        cider-known-endpoints nil
        cider-enrich-classpath t)

  (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.40.0")
  (cider-add-to-alist 'cider-jack-in-dependencies "io.github.nextjournal/clerk" "0.13.842")
  (cider-add-to-alist 'cider-jack-in-dependencies "philoskim/debux" "0.8.2")
  (cider-add-to-alist 'cider-jack-in-dependencies "com.clojure-goes-fast/clj-java-decompiler" "0.3.3")
  (cider-add-to-alist 'cider-jack-in-dependencies "criterium" "0.4.6")
  (cider-add-to-alist 'cider-jack-in-dependencies "prismatic/plumbing" "0.6.0")

  (defun +clojure/portal-open ()
    (interactive)
    (cider-interactive-eval
     "(do
(in-ns 'user)

(require '[portal.api :as portal]
         '[clojure.datafy :refer [datafy]])

(defonce portal nil)

(defn portal-submit [x]
  (portal.api/submit
   (cond-> x
     (instance? Exception x)
     (-> clojure.datafy/datafy
         (assoc :runtime :jvm)))))

;; needs graceful degredation
#_(defn portal-inspect [x]
  (portal/inspect x))

(defn portal-open! []
  (defonce portal-shutdown-hook-added
    (.addShutdownHook (Runtime/getRuntime) (Thread. #'portal/close)))
  (alter-var-root #'portal portal/open #_{:launcher :emacs})
  (add-tap #'portal-submit)
  portal)

(defn portal-close! []
  (when portal
    (alter-var-root #'portal portal/close)
    (remove-tap #'portal-submit)))

(defn portal-clear! []
  (when portal
    (portal/clear portal)))

(portal-open!))"))

  (defun +clojure/portal-clear ()
    (interactive)
    (cider-interactive-eval
     "((or (requiring-resolve 'user/portal-clear!) (constantly nil)))"))

  (defun +clojure/portal-close ()
    (interactive)
    (cider-interactive-eval
     "((or (requiring-resolve 'user/portal-close!) (constantly nil)))"))

  (defun +clojure/portal-docs ()
    (interactive)
    (cider-interactive-eval
     "((requiring-resolve 'portal.api/docs))"))

  (require 'easymenu)

  (easy-menu-define portal-menu clojure-mode-map
    "Menu for Portal (Clojure data navigator)"
    '("Portal"
      ["Open" +clojure/portal-open]
      ["Clear" +clojure/portal-clear]
      ["Close" +clojure/portal-close]
      "-"
      ["Docs" +clojure/portal-docs]))

  (map! (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
         :desc "Portal: Open"  [f8]     #'+clojure/portal-open
         :desc "Portal: Clear" [C-f8]   #'+clojure/portal-clear
         :desc "Portal: Close" [S-f8]   #'+clojure/portal-close
         :desc "Portal: Docs"  [C-S-f8] #'+clojure/portal-docs
         :desc "Portal: Docs"  [C-S-f8] #'+clojure/portal-docs
         ;; PROTIP: don't bind [C-M-f*], it's Linux shortcut to switch TTY.
         ))

  ;;;

  (defun +clojure/clerk-serve ()
    (interactive)
    (cider-interactive-eval
     "((requiring-resolve 'nextjournal.clerk/serve!) {:browse? true})"))

  (defun +clojure/clerk-show ()
    (interactive)
    (when-let ((filename (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "((requiring-resolve 'nextjournal.clerk/show!) \"" filename "\")"))))

  ;;;

  (defun +clojure/cider-jack-in-polylith (params)
    "Start an nREPL server for the current Polylith workspace and connect to it."
    (interactive "P")
    (let ((ws-dir (locate-dominating-file (pwd) "workspace.edn")))
      (if ws-dir
          (progn
            (message "Starting nREPL server from `%s'" ws-dir)
            (cider-jack-in-clj (plist-put params :project-dir ws-dir)))
        (error "Unable to locate 'workspace.edn' in current directory or parent directory"))))

  (add-to-list 'cider-connection-init-commands #'+clojure/cider-jack-in-polylith)

  (map! :map cider-mode-map
        "C-c M-k" #'+clojure/cider-jack-in-polylith)

  (map! (:map cider-repl-mode-map
         :ni "C-p" #'cider-repl-backward-input)))

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
  (defun +clojure/link-cider-session ()
    "Link the current buffer to a running CIDER session."
    (interactive)
    (setq sesman-system 'CIDER)
    (sesman-link-with-buffer)))
