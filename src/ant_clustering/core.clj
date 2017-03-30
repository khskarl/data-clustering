(ns ant-clustering.core
  (:require [quil.core :as q]
            [quil.helpers.drawing :as qd]
            [quil.helpers.seqs :as qs]
            [quil.helpers.calc :as qc]
            [ant-clustering.ant-clustering :as ac]
            [clojure.pprint :as pprint]))

(def dimensions 400)
(def tile-size (/ dimensions ac/dimension))
(def half-tile-size (/ tile-size 2))

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
  (q/background 20)
  (q/stroke-weight 0)
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 10 true))
  )

(defn draw []
  (q/background 20)
  (q/fill (q/color 90))
  (draw-entities (map entity-to-position ac/bodies))
  (q/fill (q/color 230))
  (draw-entities (map entity-to-position ac/ants))
  (ac/iterate-system)
  (q/text "Hi there" 0 0)  
  )


(q/defsketch data-clustering               
  :title "Dataaaaaaa"
  :renderer :opengl
  :setup setup           
  :draw  draw              
  :size [dimensions dimensions])   

(defn -main [] )
