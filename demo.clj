(ns clojure-west.demo
  (:use arcadia.core
        arcadia.hydrate
        arcadia.linear
        clojure-west.selection
        clojure.pprint
        clojure.repl)
  (:require
   [arcadia.internal.map-utils :as mu]
   [arcadia.introspection :as intro]
   [clojure-west.updaters :as updaters])
  (:import
   [UnityEngine
    Quaternion Vector2 Vector3 Transform GameObject Component
    Debug MeshFilter Mesh MeshRenderer Color
    LineRenderer Material Shader
    Gizmos Texture2D Resources Mathf
    Physics Ray RaycastHit
    Input
    Camera]
   [System.Reflection
    PropertyInfo MethodInfo BindingFlags]
   ;; [UnityEditor
   ;;  Selection]
   [System.Text.RegularExpressions Regex]))

(defn kill! [x]
  (let [spec (dehydrate x)]
    (destroy x)
    spec))

(def cubespec
  (kill! (create-primitive :cube)))

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

(def-compget transform UnityEngine.Transform)

;; ============================================================
;; raycasting
;; ============================================================

(defn raycast [^Ray ray]
  (when-let [^RaycastHit hit (first (RayCastHelper/raycast ray))]
    {:collider (.collider hit)
     :point (.point hit)
     :transform (.transform hit)}))

(defn forward-ray ^Ray [x]
  (let [^Transform t (transform x)]
    (Ray. (.position t) (.forward t))))

(defn raycast-forward [x]
  (raycast (forward-ray x)))

;; ============================================================
;; input
;; ============================================================

(defn keydown? [key]
  (Input/GetKeyDown (name key)))

;; ============================================================
;; activation
;; ============================================================

(defn active? [^GameObject obj]
  (.activeSelf obj))

(defn activate ^GameObject [^GameObject obj]
  (.SetActive obj true)
  obj)

(defn deactivate ^GameObject [^GameObject obj]
  (.SetActive obj false)
  obj)

(defn toggle-active ^GameObject [^GameObject obj]
  (if (active? obj)
    (deactivate obj)
    (activate obj))
  obj)

;; ============================================================
;; vr selection
;; ============================================================

;; can't use unity editor selection at all

;; (def selection
;;   (atom []))

(defn egocam-target []
  (raycast-forward egocam))

(def selection-key "d")

(defn selection-key-down? []
  (keydown? selection-key))

(defn selection-driver-behavior [this]
  (when (keydown? selection-key)
    (sel
      (when-let [^Transform t (:transform (egocam-target))]
        (.gameObject t))))
  this)

(def selection-driver
  {:state nil
   :behavior #'selection-driver-behavior})

(updaters/put-updater :selection-driver selection-driver)

;; (defn conj-single [coll x]
;;   (if (if (set? coll)
;;         (contains? coll x)
;;         (some #(= x %) coll))
;;     coll
;;     (conj coll x)))

;; (defn complete-selection [this]
;;   (mu/checked-keys [[state] this
;;                     [building-selection] state]
;;     (-> this
;;       (assoc-in [:state :selection] building-selection))))

;; (defscn ^GameObject selection-bobble
;;   (let [sb (doto (create-primitive :sphere)
;;              (set-with! [n name]
;;                "selection bobble"))]
;;     (deactivate sb)))


;; ;; really show egocam-target
;; (defn show-selection []
;;   (when-let [{:keys [point]} (egocam-target)]
;;     (do (set! (.localPosition (transform selection-bobble))
;;           point)
;;         (activate selection-bobble))))

;; (defn selecting [this]
;;   (if (selection-key-down?)
;;     (do
;;       (show-selection)
;;       this)
;;     (let [^Transform t (:transform (egocam-target))]
;;       (-> this
;;         (update-in [:state :building-selection] conj-single (.gameObject t))
;;         complete-selection))))

;; ;; need to introduce selecting-group eventually as well
;; (defn not-selecting [this]
;;   (if (selection-key-down?)
;;     (-> this
;;       (assoc-in [:state :mode] :selecting))
;;     this))

;; (defn selection-driver-behavior [this]
;;   (mu/checked-keys [[state] this
;;                     [mode] state]
;;     (case mode
;;       nil (not-selecting this)
;;       :selecting (selecting this)
;;       ;:selecting-group (selecting-group this)
;;       )))

;; (def selection-driver
;;   {:state {:mode nil
;;            :selection []
;;            :building-selection []}
;;    :behavior #'selection-driver-behavior})

;; (updaters/put-updater :selection-driver selection-driver)

;; ;; this is sort of nasty
;; (defn selection []
;;   (get-in @updaters/updaters
;;     [:updaters :selection-driver :state :selection]))


(defn controller []
  (.GetComponent (object-named "OVRPlayerController")
    UnityEngine.CharacterController))

(defn enable [x]
  (set! (.enabled x) true))

(defn disable [x]
  (set! (.enabled x) false))

(defn enable-controller []
  (set! (.enabled (controller)) true))

(defn disable-controller []
  (set! (.enabled (controller)) false))

(def repl (object-named "repl"))

(populate! repl
  {:transform
   [{:arcadia.hydrate/type UnityEngine.Transform,
     :local-position #unity/Vector3 [0.0 0.4 1],
     :local-scale #unity/Vector3 [1 1 1],
     :local-rotation
     #unity/Quaternion [0.08167479 0.008283393 -0.0006788307 0.9966244]}]})

(defn toggle-repl []
  (toggle-active repl))

(def ^GameObject egocam
  (object-named "OVRCameraRig"))

(defn in-front
  ([]
   (in-front 1))
  ([dist]
    (let [t (transform egocam)]
      (v3+
        (.position t)
        (v3* (.forward t) dist)))))


;; ============================================================
;; egocam
;; ============================================================

(def ^Camera egocam
  (.GetComponent (object-named "CenterEyeAnchor") Camera))
