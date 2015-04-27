(ns clojure-west.selection
  (:use arcadia.core)
  (:require [arcadia.introspection :as it])
  ;; assumes editor
  ;; (:import [UnityEditor Selection]
  ;;          System.Text.RegularExpressions.Regex)
  )

;; the following assumes editor, exportable version below

;; (defn objects [x]
;;   (condp instance? x
;;     String (objects-named x)
;;     Regex  (filter
;;              (fn [^UnityEngine.GameObject o]
;;                (re-find x (.name o)))
;;              (objects-typed UnityEngine.GameObject))))

;; (defn object [x]
;;   (first (objects x)))
 
;; (defn set-selection!
;;   ([x]
;;      (set! Selection/activeObject x))
;;   ([x & xs]
;;      (set! Selection/objects (into-array UnityEngine.Object (cons x xs)))))

;; (defn sel
;;   ([] (Selection/objects))
;;   ([x] (set! Selection/activeObject x))
;;   ([x & xs]
;;    (set! Selection/objects (into-array UnityEngine.Object (cons x xs)))))

;; (defn fsel []
;;   (Selection/activeObject))

;; (defn selall [coll]
;;   (set! Selection/objects
;;     (into-array UnityEngine.Object coll)))


;; hrrrm, set or vector?
;; seeeet, maybe? to make it a vector would imply some special
;; structure, and conceptually its a set; vector would have to ensure
;; deduplication. Seems more important to have fast tests for whether
;; or not something is a member of the selection. Both of those
;; together strongly indicate we're talking about a set. There might be a privileged member I guess.

(def global-selection
  (atom {}))

;; rest of this stuff should be functional I suppose.. hmmmm. Think
;; about it later, and resist urge to reify into library.
;; Can't think how to do nullary selection without galumphing global
;; state. Could make it a method of some datatype I guess, but that's
;; really not an improvement.


;; this should probably be in core, you know?
(defn objects [x]
  (condp instance? x
    String (objects-named x)
    Regex  (filter
             (fn [^UnityEngine.GameObject o]
               (re-find x (.name o)))
             (objects-typed UnityEngine.GameObject))))

(defn object [x]
  (first (objects x)))

(defn sel
  ([] @global-selection)
  ([x] (reset! global-selection #{x}))
  ([x & xs]
   (reset! global-selection (into #{} (conj x xs)))))

(defn fsel []
  (first @global-selection))

(defn selall [coll]
  (reset! global-selection (into #{} coll)))


