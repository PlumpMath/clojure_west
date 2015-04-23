(ns clojure-west.heightmap
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Gizmos]))

(def row [4 5 1 20 40 2 0 0 0 2 0 3 1 2 7 8 29 193 43 203])
(def matrix [(vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             (vec (repeatedly 80 rand))
             ])

(defn row-to-heightmap-verts [r]
  (let [size (max 1 (dec (count r)))]
    (map-indexed (fn [i y] (v2 (/ i size) y))
                 r)))

(defn matrix-to-heightmap-verts [mx]
  (let [columns (max 1 (dec (count mx)))
        row-size (dec (count (first mx)))]
    (->> mx
         (map row-to-heightmap-verts)
         (map-indexed (fn [i r] (map #(v3 (.x %) (.y %) (/ i columns)) r)))
         (apply concat))))

(defn drop-nth
  [n coll]
  (->> coll
       (map vector (iterate inc 1))
       (remove #(zero? (mod (first %) n)))
       (map second)))

(defn tri-strip
  ([width]
   (->> (range)
        (map (fn [a] [(+ a (inc width)) (+ a 1) a
                         (+ a (inc width)) (+ a (inc width) 1) (+ a 1)]))
        (drop-nth (inc width))
        (drop-last 1)
        (apply concat))))

(defn heightmap [mx]
  (let [m (UnityEngine.Mesh.)
        width (dec (count (first mx)))]
    (set! (.vertices m)
          (into-array (matrix-to-heightmap-verts mx)))
    (set! (.triangles m)
          (into-array System.Int32
                      (take (* 3 (count
                                   (matrix-to-heightmap-verts mx)))
                            (tri-strip width))))
    (.RecalculateNormals m)
    m
    ))
