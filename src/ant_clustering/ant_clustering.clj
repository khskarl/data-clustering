(ns ant-clustering.ant-clustering
  (:gen-class)  
  (:require clojure.pprint 
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; 1) Fazer leitura dos dados pra formiguinhas mortas
;; 2) Jogar formiguinhas mortas no mapa
;; 3) Tomada de decisão (problema)
;; 4) Ajuste de parâmetros

(def dimension 100)
(def radius 2)
(def num-ants 25)
(def num-bodies 1500)

(def direction-to-delta-movement
  {:up    [ 0  1]
   :right [ 1  0]
   :down  [ 0 -1]
   :left  [-1  0]})

(defn randomize-direction [direction]
  (rand-nth (direction {:up    [:up :up :up :left :right]
                        :right [:right :right :right :up :down]
                        :down  [:down :down :down :right :left]
                        :left  [:left :left :left :up :down]})))

(defn random-in-dimension []
  (rand-int dimension))

(defn random-position []
  [(random-in-dimension) (random-in-dimension)])

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

(defn get-body-ref-in-pos [[i j]]
  (:occupied-by (deref (get-tile-ref dead-grid [i j]))))

;; Ants
(defstruct ant-struct :x :y :carrying :tx :ty)

(defn compute-next-direction [ant]
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
  (ref (struct ant-struct j i nil (random-in-dimension) (random-in-dimension))))

(defn create-ants
  [num-ants]
  (dosync
   (loop [num-ants-left num-ants
          new-ants []]
     (if (zero? num-ants-left)
       new-ants
       (let [i (rand-int dimension)
             j (rand-int dimension)]                
         (recur (dec num-ants-left)
                (conj new-ants (create-ant [i j]))))))))


(def max-neighbors (dec (* (inc (* 2 radius)) (inc (* 2 radius)))))
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

(def bodies (create-bodies num-bodies))

(defn has-body-below-ant? [ant]
  (let [i (:y ant)
        j (:x ant)] 
    (is-tile-busy? dead-grid [i j])))

(defn compute-neighbors-indices
  [[i0 j0]]
  (for [i (range (- i0 radius) (inc (+ i0 radius)))
        j (range (- j0 radius) (inc (+ j0 radius)))
        :when (not (and (= i i0) (= j j0)))]
    [(wrap i)  (wrap j)]))


(def neighbors-indices (apply vector (map (fn [i]
                                            (apply vector (map (fn [j]
                                                                 (compute-neighbors-indices [i j]))
                                                               (range dimension))))
                                          (range dimension))))
(defn get-neighbors-indices
  [[i j]]
  (get-in neighbors-indices [i j]))

(defn has-reached-target? [ant-ref]
  (and (= (:x @ant-ref) (:tx @ant-ref))
       (= (:y @ant-ref) (:ty @ant-ref))))

(defn compute-new-target-position [ant-ref]
  (let [new-target (random-position)]
    (alter ant-ref assoc :tx (first new-target))
    (alter ant-ref assoc :ty (second new-target))))

(defn count-body-neighbors
  [[i j]]
  (p ::count-body-neighbors (count (filter #(:is-busy (deref (get-tile-ref dead-grid %)))
                                           (get-neighbors-indices [i j])))))

(defn chance-to-pick
  [num-neighbors] 
  (- 1 (float (/ num-neighbors max-neighbors))))

(defn chance-to-drop
  [num-neighbors] 
  (float (/ num-neighbors max-neighbors)))

(defn move-ant!
  [ant-ref [dx dy]] 
  (let [x (:x @ant-ref)
        y (:y @ant-ref)
        new-x (wrap (+ x dx))
        new-y (wrap (+ y dy))]    
    (if (has-reached-target? ant-ref)
      (compute-new-target-position ant-ref))
    (alter ant-ref assoc :x new-x :y new-y)))

(defn pick-body!
  [ant-ref body-ref]
  (let [i (:y @body-ref)
        j (:x @body-ref)
        tile-ref (get-tile-ref dead-grid [i j])] 
    (alter tile-ref assoc :occupied-by nil :is-busy false)
    (alter body-ref assoc :x dimension :y dimension)
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
        body-ref (:carrying @ant-ref)]
    ;; (println "Drop body!")
    (alter tile-ref assoc :occupied-by body-ref :is-busy true)
    (alter body-ref assoc :x j :y i)
    (alter ant-ref  assoc :carrying nil)))

;; TODO: Maybe change Ants to agents (or something else) so it can be used outside a dosync
;; TODO: read more about paralellism in clojure
;; TODO: check rich's ants

(defn decide-ant
  [ant-ref]   
  (dosync
   (let [i (:y @ant-ref)
         j (:x @ant-ref)
         has-body-below (has-body-below-ant? @ant-ref)
         is-carrying (is-ant-carrying? @ant-ref)
         chance (rand)
         num-neighbors (count-body-neighbors [i j])] 
     (if is-carrying
       (let [drop-chance (chance-to-drop num-neighbors)]
         (if (and (not has-body-below)
                  (>= drop-chance chance)) 
           (drop-body-below! ant-ref)))
       (let [pick-chance (chance-to-pick num-neighbors)]
         (if (and has-body-below
                  (>= (* pick-chance pick-chance) chance))
           (pick-body-below! ant-ref))))
     ))
  )

(defn walk-ant [ant-ref]
  (dosync
   (let [new-direction (randomize-direction (compute-next-direction @ant-ref))
         delta-movement (new-direction direction-to-delta-movement)]
     (move-ant! ant-ref delta-movement))))

(defn iterate-system [] 
  (doall (pmap walk-ant ants))
  (doall (pmap decide-ant ants)))

