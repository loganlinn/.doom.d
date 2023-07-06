; clojure-mode

(after! clojure-mode
  (setq clojure-toplevel-inside-comment-form t))

(add-hook! clojure-mode
  (setq clojure-toplevel-inside-comment-form t)
  (rainbow-delimiters-mode +1)
  (aggressive-indent-mode +1))

(add-hook! '(clojure-mode-mode-hook cider-repl-mode-hook)
           #'+loganlinn/lisp-coding-defaults)

(map! :after clojure-mode
      (:localleader
       (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
             "j"  #'+clojure/cider-jack-in-polylith
             (:when (modulep! :lang clojure +lsp)
               :desc "Clean ns" "o" #'lsp-clojure-clean-ns)
             ;; (:prefix ("d" . "debug"))
             ;; (:prefix ("e" . "eval"))
             (:prefix ("g" . "goto")
                      "t" #'projectile-find-implementation-or-test
                      [S-t] #'projectile-find-implementation-or-test-other-window
                      [M-t] #'projectile-find-implementation-or-test-other-frame)
             ;; (:prefix ("h" . "help"))
             ;; (:prefix ("i" . "inspect"))
             ;; (:prefix ("n" . "namespace"))
             ;; (:prefix ("p" . "print"))
             ;; (:prefix ("r" . "repl"))
             ;; (:prefix ("t" . "test"))
             )))

;; cider

(after! cider
  (setq cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-print-fn 'puget
        cider-repl-history-size 1000
        cider-known-endpoints nil
        cider-enrich-classpath t)

  (cider-add-to-alist 'cider-jack-in-dependencies "io.github.nextjournal/clerk" "0.13.842")
  (cider-add-to-alist 'cider-jack-in-dependencies "philoskim/debux" "0.8.2")
  (cider-add-to-alist 'cider-jack-in-dependencies "com.clojure-goes-fast/clj-java-decompiler" "0.3.3")
  (cider-add-to-alist 'cider-jack-in-dependencies "criterium" "0.4.6")
  (cider-add-to-alist 'cider-jack-in-dependencies "prismatic/plumbing" "0.6.0")

  (add-to-list 'cider-connection-init-commands #'+clojure/cider-jack-in-polylith)
  (map! (:map clojure-mode-map ;; jack-in commands belong in clojure-mode-map (NOT cider-mode-map)
         :ni "C-c M-k" #'+clojure/cider-jack-in-polylith)
        (:map cider-repl-mode-map
         :ni "C-p" #'cider-repl-backward-input)))

(after! clj-refactor
  (map! (:map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
         :ni [M-return] #'cljr-add-missing-libspec)
        (:map clojure-refactor-map
         :desc "Clean ns" "o"  #'lsp-clojure-clean-ns
         :desc "Clean ns" "C-o"  #'lsp-clojure-clean-ns))

  (setq cljr-add-ns-to-blank-clj-files t
        cljr-insert-newline-after-require t
        cljr-print-miser-width 40
        cljr-print-right-margin 80
        cljr-magic-requires :prompt
        cljr-slash-uses-suggest-libspec t
        cljr-clojure-test-declaration "[clojure.test :as test]"
        cljr-magic-require-namespaces
        '(
          ("as"       . "clojure.core.async")
          ("csv"      . "clojure.data.csv")
          ("deferred" . "manifold.deferred")
          ("edn"      . "clojure.edn")
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
          ("p"        . "plumbing.core")
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
          ("zip"      . "clojure.zip")
          )))

;; projectile

(defun projectile-root-poly-workspace-dir (dir)
  "Identify a project root in DIR by top-downsearch for Polylith workspace.edn
in dir. Return the first (topmost) matched directory or nil if not found."
  (locate-dominating-file dir "workspace.edn"))

(after! projectile
  (projectile-update-project-type 'clojure-cli :src-dir "src" :test-dir "test")

  (setq-hook! 'clojure-mode-hook
    projectile-project-root-functions
    '(projectile-root-local
      projectile-root-poly-workspace-dir
      projectile-root-bottom-up
      projectile-root-top-down
      projectile-root-top-down-recurring)))

;; tree-sitter

(after! (:and clojure-mode tree-siter)
  ;; fix for https://github.com/doomemacs/doomemacs/issues/7250
  (set-tree-sitter-lang! 'clojurec-mode 'clojure)
  (set-tree-sitter-lang! 'clojurescript-mode 'clojure))



(load! "+clojure/clj-decompiler")
(load! "+clojure/portal")
