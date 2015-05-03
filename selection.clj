(ns clojure-west.selection
  (:use arcadia.core)
  (:require [arcadia.introspection :as it]))

(def global-selection
  (atom #{}))

(defn sel
  ([] @global-selection)
  ([x] (reset! global-selection #{x}))
  ([x & xs]
   (reset! global-selection (into #{} (conj x xs)))))

(defn fsel []
  (first @global-selection))

(defn selall [coll]
  (reset! global-selection (into #{} coll)))
