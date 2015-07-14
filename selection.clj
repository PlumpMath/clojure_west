(ns clojure-west.selection
  (:use arcadia.core)
  ;;(:require [clojure-west.raycast :as ry])
  )

;; Intended mostly for live use, so emphasizing expressivity over
;; performance.

;; ============================================================
;; basics
;; ============================================================

(def global-selection
  "Global selection atom and default value of *selection*."
  (atom #{}))

(def ^:dynamic *selection*
  "Current meaning of selection state for selection functions, by
  default global-selection. Must be an atom."
  global-selection)

(defn sel
  "With no arguments, returns dereferenced current *selection*. With
  one or more arguments, sets *selection* to (empty *selection*) with
  arguments conj'ed on."
  ([] @*selection*)
  ([x]
   (swap! *selection*
     #(conj (empty %) x)))
  ([x & xs]
   (swap! *selection*
     #(into (conj (empty %) x) xs))))

(defn sel+
  "With no arguments, returns dereferenced current *selection*. With
  one or more arguments, sets value of *selection* to previous value
  with arguments conj'ed on."
  ([] @*selection*)
  ([x]
   (swap! *selection* conj x))
  ([x & xs]
   (swap! *selection* into (cons x xs))))

(defn sel-
  "With no arguments, returns dereferenced current *selection*. With
  one or more arguments, sets value of *selection* to previous value
  with arguments removed."
  ([] @*selection*)
  ([x] (swap! *selection* disj x))
  ([x & xs]
   (swap! *selection*
     #(reduce disj (disj % x) xs))))

(defn fsel
  "Returns first item in *selection*."
  []
  (first @*selection*))

(defn sel-all
  "Sets contents of *selection* atom to (empty *selection*) with
  contents of coll conj'ed on."
  [coll]
  (swap! *selection*
    #(into (empty %) coll)))

(defn sel-none
  "Sets value of *selection* to (empty *selection*)"
  []
  (swap! *selection* empty))

;; ;; ============================================================
;; ;; finesse
;; ;; ============================================================

;; (defn forward [x]
;;   (sel (ry/raycast-forward x)))

;; (defn forward+ [x]
;;   (sel+ (ry/raycast-forward x)))

;; (defn forward- [x]
;;   (sel- (ry/raycast-forward x)))
