(ns ant-clustering.ant-clustering
  (:gen-class)
  (:require clojure.pprint))

;; 1) Fazer leitura dos dados pra formiguinhas mortas
;; 2) Jogar formiguinhas mortas no mapa
;; 3) Tomada de decisão (problema)
;; 4) Ajuste de parâmetros

(def direction-to-delta-movement
  {:up    [ 0  1]
   :right [ 1  0]
   :down  [ 0 -1]
   :left  [-1  0]})

(defn random-direction []
  ((rand-nth [:up :right :down :left]) direction-to-delta-movement))

(defn randomize-direction [direction]
  (rand-nth (direction {:up    [:up :up :left :right]
                        :right [:right :right :up :down]
                        :down  [:down :down :right :left]
                        :left  [:left :left :up :down]})))

(def dimension 100)

(defn random-position []
  [(rand-int dimension) (rand-int dimension)])

(defn random-in-dimension []
  (rand-int dimension))

(defn wrap [x]
  (mod x dimension))

(defn get-tile-ref [grid [i j]]
  (get-in grid [i j]))

(defn is-tile-free? [grid [i j]]
  (= false (:is-busy (deref (get-tile-ref grid [i j])))))

(defn is-tile-busy? [grid [i j]]
  (:is-busy (deref (get-tile-ref grid [i j]))))

(defn make-grid [dimension f]
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (f)) 
                                   (range dimension)))) 
              (range dimension))))

(defstruct tile-struct :is-busy :occupied-by)

(def dead-grid  (make-grid dimension #(ref (struct tile-struct false nil))))
(def alive-grid (make-grid dimension #(ref (struct tile-struct false nil))))

(defn get-body-ref-in-pos [[i j]]
  (:occupied-by (deref (get-tile-ref dead-grid [i j]))))

;; Ants
(defstruct ant-struct :x :y :carrying :tx :ty)

(defn next-direction [ant]
  (let [dx (- (:x ant) (:tx ant))
        dy (- (:y ant) (:ty ant))]
    (if (> (Math/abs dx) (Math/abs dy))
      (if (> 0 dx)
        :right
        :left)
      (if (> 0 dy)
        :up
        :down))))

(defn is-ant-carrying?
  [ant]
  (not (nil? (:carrying ant))))

(defn create-ant
  [[i j]]
  (let [tile-ref (get-tile-ref alive-grid [i j])
        tile     @tile-ref
        ant-ref (ref (struct ant-struct j i nil (random-in-dimension) (random-in-dimension)))] 
    (ref-set tile-ref (-> tile
                          (assoc :is-busy true)
                          (assoc :occupied-by ant-ref)))
    ant-ref))

(defn create-ants
  ""
  [num-ants]
  (dosync (loop [num-ants-left num-ants
                 new-ants []]
            (if (zero? num-ants-left)
              new-ants
              (let [i (rand-int dimension)
                    j (rand-int dimension)
                    tile-busy (is-tile-busy? alive-grid [i j])]
                (if tile-busy
                  (recur num-ants-left new-ants)
                  (recur (dec num-ants-left)
                         (conj new-ants (create-ant [i j])))))))))

(def radius 1)
(def max-neighbors (dec (* (inc (* 2 radius)) (inc (* 2 radius)))))
(def num-ants 50)
(def ants (create-ants num-ants))

;; Bodies
(defstruct body-struct :x :y)

(defn create-body
  [[i j]]
  (let [tile-ref (get-tile-ref dead-grid [i j])
        tile     @tile-ref
        body-ref (ref (struct ant-struct j i))] 
    (ref-set tile-ref (-> tile
                          (assoc :is-busy true)
                          (assoc :occupied-by body-ref)))
    body-ref))

(defn create-bodies
  [num-bodies]
  (dosync (loop [num-bodies-left num-bodies
                 new-bodies []]
            (if (zero? num-bodies-left)
              new-bodies
              (let [i (rand-int dimension)
                    j (rand-int dimension)
                    grid dead-grid
                    tile-busy (is-tile-busy? grid [i j])]
                (if tile-busy
                  (recur num-bodies-left new-bodies)
                  (recur (dec num-bodies-left) (conj new-bodies (create-body [i j])))))))))

(def num-bodies 1200)
(def bodies (create-bodies num-bodies))

(defn has-body-below-ant? [ant]
  (let [i (:y ant)
        j (:x ant)] 
    (is-tile-busy? dead-grid [i j])))

(defn get-neighbors-indices
  [[i0 j0]]
  (for [i (range (- i0 radius) (inc (+ i0 radius)))
        j (range (- j0 radius) (inc (+ j0 radius)))
        :when (not (and (= i i0) (= j j0)))]
    [(wrap i)  (wrap j)]))

(defn has-reached-target? [ant-ref]
  (and (= (:x @ant-ref) (:tx @ant-ref))
       (= (:y @ant-ref) (:ty @ant-ref))))

(defn compute-new-target-position [ant-ref]
  (let [new-target (random-position)]
    (alter ant-ref assoc :tx (first new-target))
    (alter ant-ref assoc :ty (second new-target))))

(defn count-body-neighbors
  [[i j]]
  (count (filter #(:is-busy %)
                 (map #(deref (get-tile-ref dead-grid %)) (get-neighbors-indices [i j])))))

;; TODO: Implement normal distribution
;; (exp( -(x)^2 / (2 * 0.399^2) ) / sqrt(2*pi* 0.399^2) )


(defn normal-distribution [x]
  (let [sigma 0.38]
    (- 1 (/ (Math/pow Math/E (- (/ (* x x) (* 2 sigma sigma)))) (Math/sqrt (* 2 Math/PI sigma sigma))))))


(defn chance-to-pick
  [ant]
  (let [i (:y ant)
        j (:x ant)
        num-neighbors (count-body-neighbors [i j])]
    (- 1 (float (/ num-neighbors max-neighbors)))))

(defn chance-to-drop
  [ant]
  (let [i (:y ant)
        j (:x ant)
        num-neighbors (count-body-neighbors [i j])]
    (float (/ num-neighbors max-neighbors))))

(defn move-ant!
  [ant-ref [dx dy]] 
  (let [x (:x @ant-ref)
        y (:y @ant-ref)
        new-x (wrap (+ x dx))
        new-y (wrap (+ y dy))]
    (if true
      (let [tile-ref (get-tile-ref alive-grid [y x])
            tile     @tile-ref
            next-tile-ref (get-tile-ref alive-grid [new-y new-x])
            next-tile     @next-tile-ref]
        (alter tile-ref      assoc :is-busy false)
        (alter next-tile-ref assoc :is-busy true)
        (alter ant-ref assoc :x new-x)
        (alter ant-ref assoc :y new-y)
        (if (has-reached-target? ant-ref)
          (compute-new-target-position ant-ref))))))

(defn pick-body!
  [ant-ref body-ref]
  (let [i (:y (deref body-ref))
        j (:x (deref body-ref))
        tile-ref (get-tile-ref dead-grid [i j])] 
    (alter tile-ref assoc :occupied-by nil)
    (alter tile-ref assoc :is-busy false)
    (alter body-ref assoc :x dimension)
    (alter body-ref assoc :y dimension)
    (alter ant-ref  assoc :carrying body-ref)))

(defn pick-body-below!
  [ant-ref]
  (let [i (:y @ant-ref)
        j (:x @ant-ref)
        body-below (get-body-ref-in-pos [i j])]
    (pick-body! ant-ref body-below)))

(defn drop-body-below!
  [ant-ref]
  (let [i (:y @ant-ref)
        j (:x @ant-ref)
        tile-ref (get-tile-ref dead-grid [i j])
        body-ref (:carrying (deref ant-ref))] 
    (alter tile-ref assoc :occupied-by body-ref)
    (alter tile-ref assoc :is-busy true)
    (alter body-ref assoc :x j)
    (alter body-ref assoc :y i)
    (alter ant-ref  assoc :carrying nil)))

(defn iterate-ant
  [ant-ref]
  (dosync
   (let [i (:y @ant-ref)
         j (:x @ant-ref)
         has-body-below (has-body-below-ant? @ant-ref)
         is-carrying (is-ant-carrying? @ant-ref)
         chance (rand)]
     (if (and is-carrying
              (not has-body-below)
              (>= (normal-distribution (chance-to-drop @ant-ref)) chance)) 
       (drop-body-below! ant-ref)
       (if (and (not is-carrying)
                has-body-below
                (>= (chance-to-pick @ant-ref) (* chance chance)))
         (pick-body-below! ant-ref))))
   (let [delta-movement ((randomize-direction (next-direction @ant-ref)) direction-to-delta-movement)]
     (move-ant! ant-ref delta-movement))))

(defn iterate-system []
  (doall (pmap iterate-ant ants)))

