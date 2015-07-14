(ns clojure-west.selection
  (:use arcadia.core)
  (:require [clojure-west.raycast :as ry]))

;; ============================================================
;; basics
;; ============================================================

(def global-selection
  (atom #{}))

(def ^:dynamic *selection*
  global-selection)

(defn sel
  ([] @*selection*)
  ([x] (reset! *selection* #{x}))
  ([x & xs]
   (reset! *selection* (into #{x} xs))))

(defn sel+
  ([] @*selection*)
  ([x] (swap! *selection* conj x))
  ([x & xs]
   (swap! *selection* into (cons x xs))))

(defn sel-
  ([] @*selection*)
  ([x] (swap! *selection* disj x))
  ([x & xs]
   (swap! *selection*
     #(reduce disj % (cons x xs)))))

(defn selc [coll]
  (reset! *selection*
    (set coll)))

(defn fsel []
  (first @*selection*))

(defn selall [coll]
  (reset! *selection* (into #{} coll)))

(defn desel []
  (reset! *selection* #{}))

;; ============================================================
;; finesse
;; ============================================================

(defn forward [x]
  (sel (ry/raycast-forward x)))

(defn forward+ [x]
  (sel+ (ry/raycast-forward x)))

(defn forward- [x]
  (sel- (ry/raycast-forward x)))
