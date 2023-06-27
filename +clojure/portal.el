;;; +clojure/portal.el -*- lexical-binding: t; -*-

(require 'cider)
(require 'easymenu)

(after! cider
  (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.40.0")

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
