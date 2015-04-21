(ns clojure-west.selection
  (:use arcadia.core)
  (:require [arcadia.introspection :as it])
  (:import [UnityEditor Selection]
           System.Text.RegularExpressions.Regex))

(defn objects [x]
  (condp instance? x
    String (objects-named x)
    Regex  (filter
             (fn [^UnityEngine.GameObject o]
               (re-find x (.name o)))
             (objects-typed UnityEngine.GameObject))))

(defn object [x]
  (first (objects x)))
 
;; (defn selected-object []
;;   (Selection/activeObject))

;; (defn selected-objects []
;;   (Selection/objects))

(defn set-selection!
  ([x]
     (set! Selection/activeObject x))
  ([x & xs]
     (set! Selection/objects (into-array UnityEngine.Object (cons x xs)))))

(defn sel
  ([] (Selection/objects))
  ([x] (set! Selection/activeObject x))
  ([x & xs]
   (set! Selection/objects (into-array UnityEngine.Object (cons x xs)))))

(defn fsel []
  (Selection/activeObject))

(defn selall [coll]
  (set! Selection/objects
    (into-array UnityEngine.Object coll)))
