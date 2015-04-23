(ns clojure-west.updaters
  (:use arcadia.core)
  (:require [arcadia.internal.map-utils :as mu]
            arcadia.inspectors))

(def updaters
  (atom
    {:disabled #{}
     :updaters {}}))

(defn put-updater [k updr]
  (swap! updaters update :updaters assoc k updr))

(defn run-updaters [updater]
  (mu/checked-keys [[disabled updaters] updater]
    (doseq [k (remove disabled (keys updaters))]
      (try
        ((k updaters))
        (catch Exception e
          (println (str e)))))))

(defcomponent Updater []
  (Update [this]
    (run-updaters @updaters)))
