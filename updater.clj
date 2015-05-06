(ns clojure-west.updater
  (:use arcadia.core)
  (:require [arcadia.internal.map-utils :as mu]))

(def disabled
  (atom #{}))

(def updater-fns
  (atom {}))

(defn put [k updr]
  (swap! updater-fns assoc k updr))

(defn disable [k]
  (swap! disabled conj k))

(defn enable [k]
  (swap! disabled disj k))

(defn toggle [k]
  (swap! disabled #(if (% k) (disj % k) (conj % k))))

(defn run-updaters []
  (let [ds @disabled]
   (doseq [[k f] @updater-fns
           :when (not (ds k))]
     (f))))

(defcomponent Updater []
  (Update [this]
    (run-updaters)))
