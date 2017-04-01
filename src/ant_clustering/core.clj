(ns ant-clustering.core
  (:require [quil.core :as q]
            [quil.helpers.drawing :as qd]
            [quil.helpers.seqs :as qs]
            [quil.helpers.calc :as qc]
            [ant-clustering.ant-clustering :as ac]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [clojure.pprint :as pprint]))

(tufte/add-basic-println-handler! {})

(profile {} (dotimes [_ 500]
              (ac/iterate-system)))

(def dimensions 400)
(def tile-size (/ dimensions ac/dimension))
(def half-tile-size (/ tile-size 2))
(def iterations-per-frame 20)

(def current-iteration (atom 0))

(defn grid-to-screen [[x y]]
  [(+ (* tile-size x) half-tile-size)
   (+ (* tile-size y) half-tile-size)])

(defn draw-entity [position]
  (let [x (first position)
        y (second position)]
    (q/ellipse x y tile-size tile-size)))

(defn draw-entities [positions]
  (run! draw-entity (map grid-to-screen positions)))

(defn entity-to-position [entity]
  [(:x @entity) (:y @entity)])

(defn setup []
  (q/background 20 20 20 20)
  (q/stroke-weight 0)
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 10 true)))

(defn normal-distribution [x]
  (let [sigma 0.38]
    (- 1 (/ (Math/pow Math/E (- (/ (* x x) (* 2 sigma sigma))))
            (Math/sqrt (* 2 Math/PI sigma sigma))))))

(defn draw-gaussian []
  (q/fill 200 150 125)
  (q/with-translation [0 (q/width)]
    (doseq [t (range 0 1 0.005)]
      (let [x t
            y (* (normal-distribution t))]
        (q/ellipse (* x 400) (- (* y 400)) 2 2)))))

(defn draw []
  (q/background 20)
  
  (q/fill 20 20 20 40)
  (q/rect 0 0 dimensions dimensions)
  (q/fill 90)
  (draw-entities (map entity-to-position ac/bodies))
  (q/fill 230)
  (draw-entities (map entity-to-position ac/ants))
  (dotimes [_ iterations-per-frame]
    (swap! current-iteration inc)
    (ac/iterate-system))

  ;; (draw-gaussian)
  
  (q/fill 255)
  (q/text (str (q/current-frame-rate)) 0 dimensions)
  (q/text (str @current-iteration) 0 (- dimensions 10))  
  )

()

(q/defsketch data-clustering               
  :title "Dataaaaaaa"
  :renderer :opengl
  :setup setup           
  :draw  draw              
  :size [dimensions dimensions])   

(defn -main [] )

