;;; +clojure.el -*- lexical-binding: t; -*-

;; https://github.com/doomemacs/doomemacs/pull/7079
;; (setq +clojure-load-clj-refactor-with-lsp t)

(after! clojure
  (add-hook 'clojure-mode-hook #'format-all-mode))

(map! (:after clojure-mode
       :map clojure-mode-map
       (:localleader
        "j"  #'+clojure/cider-jack-in-polylith
        ;; (:prefix ("d" . "debug"))
        ;; (:prefix ("e" . "eval"))
        ;; (:prefix ("h" . "help"))
        ;; (:prefix ("i" . "inspect"))
        ;; (:prefix ("n" . "namespace"))
        ;; (:prefix ("p" . "print"))
        ;; (:prefix ("r" . "repl"))
        ;; (:prefix ("t" . "test"))
        (:prefix ("g" . "goto")
                 "t" #'projectile-find-implementation-or-test
                 [S-t] #'projectile-find-implementation-or-test-other-window
                 [M-t] #'projectile-find-implementation-or-test-other-frame)
        (:prefix ("i" . "inspect")
                 "i" #'cider-inspector-pop
                 "e" #'cider-inspect-expr
                 "d" #'cider-inspect-defun-at-point
                 ))

       (:after lsp-mode
        :localleader
        :desc "Clean ns" "o" #'lsp-clojure-clean-ns))

      (:after cider
       :map cider-repl-mode-map
       :ni "C-p" #'cider-repl-backward-input
       :ni "C-n" #'cider-repl-forward-input
       :n "," #'cider-repl-handle-shortcut
       :ni "M-," #'cider-repl-handle-shortcut)

      (:after cider
       :map clojure-mode-map
       :ni "M-," #'+cider/open-shortcuts
       (:localleader
        "l" #'+cider/link-session))

      (:after clj-refactor
       :map clojure-refactor-map
       :desc "Clean ns" "o"    #'lsp-clojure-clean-ns
       :desc "Clean ns" "C-o"  #'lsp-clojure-clean-ns))

(setq-hook! clojure-mode
  projectile-project-root-functions
  '(projectile-root-local
    +polylith/projectile-root-dir
    projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring))

(after! cider
  (set-lookup-handlers! 'cider-mode :documentation #'cider-doc-lookup)

  (setq cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-ns-save-files-on-refresh t
        cider-print-fn 'puget
        cider-repl-history-size 1000
        cider-known-endpoints nil
        cider-enrich-classpath t
        cider-completion-annotations-include-ns t
        cider-auto-select-error-buffer t
        cider-auto-select-test-report-buffer t
        cider-auto-jump-to-error t)

  (add-to-list 'cider-connection-init-commands #'+clojure/cider-jack-in-polylith)

  ;; TODO use [parseedn](https://github.com/clojure-emacs/parseedn)
  ;;      to read these from a `deps.edn'
  (cider-add-to-alist 'cider-jack-in-dependencies "io.github.nextjournal/clerk" "0.13.842")
  (cider-add-to-alist 'cider-jack-in-dependencies "philoskim/debux" "0.8.2")
  (cider-add-to-alist 'cider-jack-in-dependencies "com.clojure-goes-fast/clj-java-decompiler" "0.3.3")
  (cider-add-to-alist 'cider-jack-in-dependencies "criterium" "0.4.6")
  (cider-add-to-alist 'cider-jack-in-dependencies "prismatic/schema" "1.4.1")
  (cider-add-to-alist 'cider-jack-in-dependencies "prismatic/plumbing" "0.6.0")

  ;; shortcuts menu 😎
  (cider-repl-add-shortcut "ns-user" (cmd!! (cider-repl-set-ns 'user))))

(after! clj-refactor
  (setq cljr-add-ns-to-blank-clj-files t
        cljr-insert-newline-after-require t
        cljr-print-miser-width 40
        cljr-print-right-margin 80
        cljr-magic-requires :prompt
        cljr-slash-uses-suggest-libspec t
        cljr-clojure-test-declaration "[clojure.test :as test]"
        cljr-magic-require-namespaces
        '(("as"       . "clojure.core.async")
          ("csv"      . "clojure.data.csv")
          ("cli"      . "babashka.cli")
          ("deferred" . "manifold.deferred")
          ("edn"      . "clojure.edn")
          ("fs"       . "babshka.fs")
          ("http"     . "clj-http.client")
          ("io"       . "clojure.java.io")
          ("json"     . "cheshire.core")
          ("jt"       . "java-time")
          ("log"      . "clojure.tools.logging")
          ("m"        . "malli.core")
          ("mat"      . "clojure.core.matrix")
          ("me"       . "malli.error")
          ("mi"       . "malli.instrument")
          ("mr"       . "malli.registry")
          ("mt"       . "malli.transform")
          ("mu"       . "malli.util")
          ("nrepl"    . "clojure.nrepl")
          ("process"  . "babashka.process")
          ("pp"       . "clojure.pprint")
          ("s"        . "clojure.spec.alpha")
          ("set"      . "clojure.set")
          ("sh"       . "clojure.java.shell")
          ("spec"     . "clojure.spec.alpha")
          ("sql"      . "honey.sql")
          ("sqlh"     . "honey.sql.helpers")
          ("str"      . "clojure.string")
          ("stream"   . "manifold.stream")
          ("walk"     . "clojure.walk")
          ("xml"      . "clojure.data.xml")
          ("yaml"     . "clj-yaml.core")
          ("zip"      . "clojure.zip"))))

(after! projectile
  (projectile-update-project-type 'clojure-cli :src-dir "src" :test-dir "test")
  (add-to-list 'projectile-other-file-alist '("clj" . ("cljc" "cljs")))
  (add-to-list 'projectile-other-file-alist '("cljs" . ("cljc" "clj")))
  (add-to-list 'projectile-other-file-alist '("cljc" . ("clj" "cljs")))
  (add-to-list 'projectile-other-file-alist '("edn" . ("clj"))))

(load! "+clojure/clj-decompiler")
(load! "+clojure/portal")
