(ns clojure-west.mesh
  (:use [arcadia.core arcadia.linear])
  (:import [UnityEngine Mesh Vector3]
           [LibTessDotNet Tess ContourVertex]
           [UnityEngine MeshFilter MeshRenderer
            Debug GameObject Component
            Mesh Vector3 Vector2
            Mathf]))

;; ============================================================
;; utils
;; ============================================================

(defn max-by [keyfn coll]
  (when-let [x (first coll)]
    (first
      (reduce
        (fn [[x kx :as prev] y]
          (let [ky (keyfn y)]
            (if (< kx ky)
              [y ky]
              prev)))
        [x (keyfn x)]
        (rest coll)))))

(defn min-by [keyfn coll]
  (when-let [x (first coll)]
    (first
      (reduce
        (fn [[x kx :as prev] y]
          (let [ky (keyfn y)]
            (if (> kx ky)
              [y ky]
              prev)))
        [x (keyfn x)]
        (rest coll)))))

(defn delete-duplicates
  "Returns a lazy sequence removing duplicates in coll.
  Returns a transducer when no collection is provided."
  ([]
   (fn [rf]
     (let [pv (volatile! #{})] ;; TODO: use faster imperative datastructure
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv]
            (if (prior input)
              result
              (do (vreset! pv (conj prior input))
                  (rf result input)))))))))
  ([coll] (sequence (delete-duplicates) coll)))

(defn index-map [coll]
  (zipmap coll (range)))

(defn wrap-1 [coll]
  (concat coll (take 1 coll)))

;; ============================================================
;; v3 
;; ============================================================

(defn v3-max-min [v3coll]
  [(v3
     (apply max
       (map (fn [^Vector3 v] (.x v))
         v3coll))
     (apply max
       (map (fn [^Vector3 v] (.y v))
         v3coll))
     (apply max
       (map (fn [^Vector3 v] (.z v))
         v3coll)))
   (v3
     (apply min
       (map (fn [^Vector3 v] (.x v))
         v3coll))
     (apply min
       (map (fn [^Vector3 v] (.y v))
         v3coll))
     (apply min
       (map (fn [^Vector3 v] (.z v))
         v3coll)))])

;; ============================================================
;; backface
;; ============================================================

;; based on http://answers.unity3d.com/questions/280741/how-make-visible-the-back-face-of-a-mesh.html
(defn backfaced-mesh ^Mesh [^Mesh mesh]
  (let [vs (.vertices mesh)
        szv (count vs)
        uvs (.uv mesh)
        szu (count uvs)
        norms (.normals mesh)
        szn (count norms)
        tris (.triangles mesh)
        szt (count tris) 
        ^|UnityEngine.Vector3[]| new-vs (make-array Vector3 (* szv 2)) 
        ^|UnityEngine.Vector2[]| new-uvs (make-array Vector2 (* szu 2))
        ^|UnityEngine.Vector3[]| new-norms (make-array Vector3 (* szn 2))
        ^|System.Int32[]| new-tris (int-array (* szt 2))]
    (dotimes [j szv]
      (let [^Vector3 v (aget vs j)]
        (aset new-vs j v)
        (aset new-vs (+ j szv) v))
      (let [^Vector2 uv (aget uvs j)]
        (aset new-uvs j uv)
        (aset new-uvs (+ j szv) uv))
      (let [n (aget norms j)]
        (aset new-norms j n)
        (aset new-norms (+ j szv) (v3- n))))
    (loop [i (int 0)]
      (when (< i szt)
        (aset new-tris i       (aget tris i))
        (aset new-tris (+ 1 i) (aget tris (+ 1 i)))
        (aset new-tris (+ 2 i) (aget tris (+ 2 i)))
        (let [j (+ i szt)]
          (aset new-tris j       (+ (aget tris i)       szv))
          (aset new-tris (+ 1 j) (+ (aget tris (+ 2 i)) szv))
          (aset new-tris (+ 2 j) (+ (aget tris (+ 1 i)) szv)))
        (recur (+ i 3))))
    (let [mesh2 (Mesh.)]
      (set! (.vertices mesh2) new-vs)
      (set! (.uv mesh2) new-uvs)
      (set! (.normals mesh2) new-norms)
      (set! (.triangles mesh2) new-tris) ;; evidently important to do this last
      mesh2)))

;; ============================================================
;; LibTess interop
;; ============================================================

(defn- tessv-to-unityv ^UnityEngine.Vector3 [^LibTessDotNet.Vec3 v]
  (v3 (.X v) (.Y v) (.Z v)))

(defn- unityv-to-tessv ^LibTessDotNet.Vec3 [^UnityEngine.Vector3 v]
  (LibTessDotNet.Vec3/qwikVec3 (.x v) (.y v) (.z v)))

(defn- unityv-to-contourv ^ContourVertex [^Vector3 v]
  (let [^ContourVertex cv (ContourVertex.)]
    (set! (.Position cv) (unityv-to-tessv v))
    cv))

(defn- triangulate-tess ^Tess [v3s] 
  (let [^|LibTessDotNet.ContourVertex[]| contour (->> v3s
                                                   (map unityv-to-contourv)
                                                   (into-array LibTessDotNet.ContourVertex))]
   
    (let [^Tess tess (Tess.)]
      (.AddContour tess contour) ;; can add some winding specification thing as extra arg here
      (.Tessellate tess
        LibTessDotNet.WindingRule/EvenOdd,
        LibTessDotNet.ElementType/Polygons,
        3)
      tess)))

;; ============================================================
;; polygon triangulation
;; ============================================================

(defn triangulate ^Mesh [v3s]
  (let [^Tess tess (triangulate-tess v3s)
        ^|UnityEngine.Vector3[]| vs (->> (.Vertices tess)
                                      (map (fn [^ContourVertex v]
                                             (tessv-to-unityv (.Position v))))
                                      (into-array Vector3))
        ^|UnityEngine.Vector2[]| uvs (into-array |UnityEngine.Vector2|
                                       (take (count vs) ;; stupid for now
                                         (cycle [(v2 0 0) (v2 0 1) (v2 1 0)])))
        ^Mesh mesh (Mesh.)]
    (set! (.vertices mesh) vs)
    (set! (.uv mesh) uvs)
    (set! (.triangles mesh) (.Elements tess))
    (.RecalculateNormals mesh)
    mesh))
