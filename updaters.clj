(ns clojure-west.updaters
  (:use arcadia.core)
  (:require [arcadia.internal.map-utils :as mu]))

(def updaters
  (atom
    {:disabled #{}
     :updaters {}}))

(defn run-updaters [updater]
  (mu/checked-keys [[disabled updaters] updater]
    (doseq [k (remove disabled (keys updaters))]
      (try
        ((k updaters))
        (catch Exception e
          (Debug/Log (str e)))))))

(defcomponent Updater []
  (Update [this]
    (run-updaters @updaters)))
