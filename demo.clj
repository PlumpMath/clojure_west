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
   [clojure-west.updater :as updater]
   )
  (:import
   [UnityEngine
    Quaternion Vector2 Vector3 Transform GameObject Component
    Debug MeshFilter Mesh MeshRenderer Color
    LineRenderer Material Shader
    Gizmos Texture2D Resources Mathf
    Physics Ray RaycastHit
    Input
    Camera
    Application]
   [System.Reflection
    PropertyInfo MethodInfo BindingFlags]
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

;; should be inlining, obviously
(definline ^GameObject game-object [x]
  `(condcast-> ~x x#
     UnityEngine.GameObject x#
     UnityEngine.Component (.gameObject x#)))

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
;; egocam
;; ============================================================

(defn ^GameObject egocam []
  (object-named "CenterEyeAnchor"))

;; ============================================================
;; vr selection
;; ============================================================
;; really quick and dirty
;; can't use unity editor selection at all

(defn egocam-target []
  (raycast-forward (egocam)))

(def selection-key "d")

(defn selection-key-down? []
  (keydown? selection-key))

(defn selection-driver []
  (when (keydown? selection-key)
    (sel
      (when-let [^Transform t (:transform (egocam-target))]
        (.gameObject t)))))


;; ============================================================
;; controller, vr camera stuff
;; ============================================================

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

(def ^GameObject left-eye-anchor
  (object-named "LeftEyeAnchor"))

(def ^GameObject center-eye-anchor
  (object-named "CenterEyeAnchor"))

(def ^GameObject right-eye-anchor
  (object-named "RightEyeAnchor"))

;; ============================================================
;; repl positioning etc

(defn in-front
  ([]
   (in-front 1))
  ([dist]
    (let [t (transform (egocam))]
      (v3+
        (.position t)
        (v3* (.forward t) dist)))))

(defn ^GameObject repl []
  (object-named "repl"))

(defn reptog []
  (toggle-active (repl)))

(defn reposition-repl []
  (let [^Transform t (transform (egocam))]
    (populate! repl
      {:transform [{:local-position (in-front)
                    :local-scale (v3 1 1 1)
                    :local-rotation (v3 0 0 0)}]})))

;; ============================================================
;; pivot

;; (defn point-pivot ^Vector3 [^Vector3 target, ^Vector3 piv, ^Quaternion rot]
;;   (v3+ (qv* rot (v3- target piv)) piv))

(defn pivot-local-transform! ^Transform [^Transform trns, piv, rot]
  (doto trns
    (set-with! [lr localRotation]
      (qq* rot lr))
    (set-with! [lp localPosition]
      (point-pivot lp piv rot))))

(defn pivot-transform! ^Transform [^Transform trns, piv, rot]
  (doto trns
    (set-with! [r rotation]
      (qq* rot r))
    (set-with! [p Position]
      (point-pivot p piv rot))))

(defn pivot! [target piv rot]
  (pivot-transform! (transform target) piv rot)
  target)

;; ============================================================
;; test some
;; ============================================================

(def screenshot-counter
  (atom 0))

(defn screen-nabber
  ([]
   (screen-nabber
     (str "screenshot_" (swap! screenshot-counter inc))))
  ([filename]
   (Application/CaptureScreenshot filename)))

(declare oculus-opening-grabber-update)

(defcomponent OculusOpeningGrabber [^Int32 i]
  (Start [this]
    (oculus-opening-grabber-update this))
  (Awake [this]
    (oculus-opening-grabber-update this))
  (Update [this]
    (oculus-opening-grabber-update this)))

(defn oculus-opening-grabber-update [^OculusOpeningGrabber this]
  (let [i (.i this)]
    (when (< i 40)
      (screen-nabber)
      (set! (.i this) (inc i)))
    this))

