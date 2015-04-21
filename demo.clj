(ns clojure-west.demo
  (:use arcadia.core
        arcadia.hydrate
        clojure-west.selection
        clojure.pprint
        clojure.repl)
  (:require
   [arcadia.internal.map-utils :as mu])
  (:import
   [UnityEngine
    Quaternion Vector2 Vector3 Transform GameObject Component
    Debug MeshFilter Mesh MeshRenderer Color
    LineRenderer Material Shader
    Gizmos Texture2D Resources Mathf]
   [System.Reflection
    PropertyInfo MethodInfo BindingFlags]
   [UnityEditor
    Selection]
   [System.Text.RegularExpressions Regex]))

;; ============================================================
;; processing
;; ============================================================

(defn domap [f xs]
  (doseq [x xs] (f x)))

;; ============================================================
;; misc nonsense
;; ============================================================

(defn obj-children [^GameObject obj]
  (map (fn [^Transform t] (.gameObject t))
    (get-component obj Transform)))

(defmacro set-with! [obj [sym & props] & body]
  `(let [obj# ~obj
         ~sym (.. obj# ~@props)]
     (set! (.. obj# ~@props) (do ~@body))))

(defn destroy-newbs []
  (domap destroy (objects "New Game Object")))

;; make this take opts, and prepend by default rather than overwrite
(defmacro poop [& body]
  `(spit "scratch.txt"
     (with-out-str
       ~@body)))

;; this could be made into optimized inlining thing when we build out
;; the type matching machinery a bit more
(defmacro def-compget [name typesym]
  (let [objsym (gensym "obj_")]
    `(defn ~name ~(with-meta [objsym] {:tag typesym}) ;; soon this tag will be important
       (get-component ~objsym ~typesym))))

;; yeah!
;; this could even autotag with the actual type !!OMG
(defmacro def-selected [name]
  `(def ~name (selected-object)))
