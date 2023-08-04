(ns user.portal
  (:require
   [portal.api :as portal]
   [clojure.datafy :refer [datafy]]))

(defonce portal nil)

(defn submit [x]
  (portal.api/submit
   (cond-> x
     (instance? Exception x)
     (-> clojure.datafy/datafy
         (assoc :runtime :jvm)))))

(defn open! []
  (defonce portal-shutdown-hook-added
    (.addShutdownHook (Runtime/getRuntime) (Thread. #'portal/close)))

  (alter-var-root #'portal portal/open #_{:launcher :emacs})
  (add-tap #'submit)
  portal)

(defn close! []
  (when portal
    (alter-var-root #'portal portal/close)
    (remove-tap #'submit)))

(defn clear! []
  (when portal
    (portal/clear portal)))
