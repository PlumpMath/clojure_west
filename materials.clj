(ns clojure-west.materials
  (import [UnityEngine
           MeshFilter MeshRenderer Shader
           Material GameObject Component
           Color]))

(defn shader-material ^Material [^String name]
  (Material. (Shader/Find name)))

(defn- ensured-mesh-renderer ^MeshRenderer [^GameObject obj]
  (or (.GetComponent obj UnityEngine.MeshRenderer)
    (.AddComponent obj UnityEngine.MeshRenderer)))

(defn color ^Color
  ([^GameObject obj]
   (when-let [^MeshRenderer mr (.GetComponent obj UnityEngine.MeshRenderer)]
     (.. mr material color)))
  ([^GameObject obj, ^Color c]
   (let [^MeshRenderer mr (ensured-mesh-renderer obj)]
     (set! (.. mr material color) c))
   obj))

(defn- gentagged
  ([tag]
   (gentagged "" tag))
  ([prefix tag]
   (with-meta (gensym prefix) {:tag tag})))

;; TODO: make inlining, game-object-ifying, etc etc etc
(defmacro ^:private def-color-set-get [name argn fieldsym]
  (let [csym (gensym "color_") ;; can't typehint local with primitive initializer blaaaa
        msym (gentagged "material_" 'UnityEngine.Material)
        setform `(set! (.color ~msym)
                   (UnityEngine.Color.
                     ~@(assoc `[(.r ~csym) (.g ~csym) (.b ~csym) (.a ~csym)]
                         (condp = fieldsym
                           'r 0
                           'g 1
                           'b 2
                           'a 3)
                         argn)))]
    `(defn ~name
       ([^GameObject obj#]
        (when-let [^MeshRenderer mr# (.GetComponent obj# UnityEngine.MeshRenderer)]
          (. (.color mr#) ~fieldsym)))
       (^GameObject [^GameObject obj#, ~argn]
         (when-let [^MeshRenderer mr# (.GetComponent obj# UnityEngine.MeshRenderer)]
           (let [^Material ~msym (.material mr#)
                 ~csym (.color ~msym)] ;; primitive initializer horseshit here
             ~setform))
         obj#))))

(def-color-set-get red r r)

(def-color-set-get green g g)

(def-color-set-get blue b b)

(def-color-set-get alpha a a)

(defn set-vertex-color ^GameObject [^GameObject obj, ^Color c]
  (let [^MeshFilter mr (.GetComponent obj UnityEngine.MeshFilter)
        m (.mesh mr)
        ^|UnityEngine.Color[]| car (make-array Color (.vertexCount m))]
    (dotimes [i (count car)] (aset car i c))
    (set! (.. mr mesh colors) car)
    obj))

;; should make this consistent, extend getter-setter schmuppity to
;; other things

(defn set-shared-material ^GameObject [^GameObject obj, ^Material m]
  (let [^MeshRenderer mr (ensured-mesh-renderer obj)]
    (set! (.sharedMaterial mr) m)
    obj))

(defn set-material ^GameObject [^GameObject obj, ^Material m]
  (let [^MeshRenderer mr (ensured-mesh-renderer obj)]
    (set! (.material mr) m)
    obj))

(defn set-diffuse-color ^GameObject [^GameObject obj, ^Color c]
  (-> obj
    (set-material (Material. (Shader/Find "Diffuse")))
    (color c)))
