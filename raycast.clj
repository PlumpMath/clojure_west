(ns clojure-west.raycast
  (:use arcadia.core)
  (:import [UnityEngine Ray RaycastHit Transform]))

;; ============================================================
;; raycasting
;; ============================================================

;; this is all foolishness, of course (see RayCastHelper for proof);
;; we construct a pointless array of RaycastHits to pass
;; back. Something to do with a bug in Clojure-CLR if I recall.

(defn raycast ^RaycastHit [^Ray ray]
  (first (RayCastHelper/raycast ray)))

(defn forward-ray ^Ray [x]
  (let [^Transform t (get-component x Transform)]
    (Ray. (.position t) (.forward t))))

(defn raycast-forward ^RaycastHit [x]
  (raycast (forward-ray x)))
