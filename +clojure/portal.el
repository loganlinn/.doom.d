;;; +clojure/portal.el -*- lexical-binding: t; -*-

(after! cider
  (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.44.0")

  (defun +clojure/portal-open ()
    (interactive)
    ;; TODO probably can add doom dir to class path for a normal require
    (cider-load-buffer
     (find-file-noselect
      (file-name-concat doom-user-dir "+clojure" "src" "user" "portal.clj")))
    (cider-interactive-eval "(user.portal/open!)"))

  (defun +clojure/portal-clear ()
    (interactive)
    (cider-interactive-eval
     "((or (requiring-resolve 'user.portal/clear!) (constantly nil)))"))

  (defun +clojure/portal-close ()
    (interactive)
    (cider-interactive-eval
     "((or (requiring-resolve 'user.portal/close!) (constantly nil)))"))

  (defun +clojure/portal-docs ()
    (interactive)
    (cider-interactive-eval
     "((requiring-resolve 'portal.api/docs))"))


  (easy-menu-define portal-menu clojure-mode-map
    "Menu for Portal (Clojure data navigator)"
    '("Portal"
      ["Open" +clojure/portal-open]
      ["Clear" +clojure/portal-clear]
      ["Close" +clojure/portal-close]
      "-"
      ["Docs" +clojure/portal-docs]))

  (map! :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
        :desc "Portal: Open"  [f8]     #'+clojure/portal-open
        :desc "Portal: Clear" [C-f8]   #'+clojure/portal-clear
        :desc "Portal: Close" [S-f8]   #'+clojure/portal-close
        :desc "Portal: Docs"  [C-S-f8] #'+clojure/portal-docs
        :desc "Portal: Docs"  [C-S-f8] #'+clojure/portal-docs))
