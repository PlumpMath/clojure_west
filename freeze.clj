(ns clojure-west.freeze
  (:use arcadia.core arcadia.linear)
  (:require [arcadia.hydrate :as h])
  (:import [UnityEngine Rigidbody GameObject]))

;; should be in core
(defn- game-object-seq [x]
  (if (instance? GameObject x)
    (tree-seq
      (constantly true) ;; composed only of game objects soooo
      (fn [^GameObject y]
        (for [^Transform tr (.transform y)]
          (.gameObject tr)))
      x)
    (throw
      (ArgumentException.
        (str "Expects instance of GameObject, instead got instance of "
          (class x))))))

(defcomponent FrozenState [state])

(defn frozen? [^GameObject obj]
  (boolean
    (get-component obj FrozenState)))

(defn freeze ^GameObject [^GameObject obj]
  (when-not (frozen? obj)
    (when-let [^Rigidbody rb (get-component obj Rigidbody)]
      (let [^FrozenState fs (add-component obj FrozenState)]
        (set! (.state fs)
          (h/dehydrate rb))
        (set! (.isKinematic rb) true)
        (set! (.useGravity rb) false)
        (set! (.velocity rb) (v3 0)))))
  obj)

;; maybe pooling/reusing FrozenState's would be better
(defn thaw ^GameObject [^GameObject obj]
  (when-let [^Rigidbody rb (get-component obj Rigidbody)]
    (when-let [^FrozenState fs (get-component obj FrozenState)]
      (h/populate! rb
        (.state fs))
      (destroy fs)))
  obj)

(defn deep-freeze ^GameObject [^GameObject obj]
  (doseq [obj (game-object-seq obj)]
    (freeze obj))
  obj)

(defn deep-thaw ^GameObject [^GameObject obj]
  (doseq [obj (game-object-seq obj)]
    (thaw obj))
  obj)
