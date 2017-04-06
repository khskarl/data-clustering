(ns ant-clustering.core
  (:require [quil.core :as q]
            [quil.helpers.drawing :as qd]
            [quil.helpers.seqs :as qs]
            [quil.helpers.calc :as qc]
            [ant-clustering.ant-clustering :as ac]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [clojure.pprint :as pprint]))

(tufte/add-basic-println-handler! {})

;; (profile {} (dotimes [_ 500]
;;               (ac/iterate-system)))

;; (defn normal-distribution [x]
;;   (let [sigma 0.38]
;;     (- 1 (/ (Math/pow Math/E (- (/ (* x x) (* 2 sigma sigma))))
;;             (Math/sqrt (* 2 Math/PI sigma sigma))))))

;; (defn draw-gaussian []
;;   (q/fill 200 150 125)
;;   (q/with-translation [0 (q/width)]
;;     (doseq [t (range 0 1 0.005)]
;;       (let [x t
;;             y (* (normal-distribution t))]
;;         (q/ellipse (* x 400) (- (* y 400)) 2 2)))))

(def dimensions 400)
(def tile-size (/ dimensions ac/dimension))
(def half-tile-size (/ tile-size 2))
(def iterations-per-frame 100)

(def current-iteration (atom 0))

(def class-colors [0xff808080
                   0xffDC143C 0xff9ACD32 0xff3296DC 0xff9932CC
                   0xffFF0000 0xff00FF00 0xff0000FF 0xffFFFF00
                   0xffAA0000 0xff00AA00 0xff0000AA 0xff00FFFF
                   0xff660000 0xff006600 0xff000066 0xff660066])

(defn class-colors-lazy
  ([] (class-colors-lazy 0))
  ([hue] (lazy-seq (cons [(mod hue 255) 200 230 255] (class-colors-lazy (+ hue 17))))))

(nth (class-colors-lazy) 15)

(defn discrete-to-screen [x]
  (+ (* tile-size x) half-tile-size))

(defn grid-to-screen [[x y]]
  [(discrete-to-screen x)
   (discrete-to-screen y)])

(defn draw-entity [position]
  (let [x (first position)
        y (second position)]
    (q/ellipse x y (* tile-size 0.8) (* tile-size 0.8))))

(defn draw-entities [positions]
  (run! draw-entity (map grid-to-screen positions)))

(defn entity-to-position [entity]
  [(:x @entity) (:y @entity)])

(defn draw-body [id body-ref]
  (let [x (discrete-to-screen (:x @body-ref))
        y (discrete-to-screen (:y @body-ref))
        class (nth ac/dataset-classes id)]
    (q/fill (nth class-colors class)) 
    ;;(apply q/fill (nth (class-colors-lazy) class))
    (q/ellipse x y tile-size tile-size)))


(defn setup []
  (q/background 20 20 20 20)
  (q/stroke-weight 0)
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 10 true))
  ;;(q/color-mode :hsb)
  )

(defn draw []
  
  (q/background 20)
  (q/fill 20 20 20 10)
  (q/rect 0 0 dimensions dimensions)
  (doall
   (map-indexed draw-body ac/bodies))

  (q/fill 230)
  (draw-entities (map entity-to-position ac/ants))

  (if (= 0 (mod @current-iteration 25000))
    (q/save-frame
     (str "screenshots/" (apply max ac/dataset-classes) "_" @current-iteration ".png"))) 
  
  

  (q/fill 255)
  (q/text (str (q/current-frame-rate)) 0 dimensions)
  (q/text (str @current-iteration) 0 (- dimensions 10))

  (dotimes [_ iterations-per-frame]
    (swap! current-iteration inc)
    (ac/iterate-system))

  )


(q/defsketch data-clustering               
  :title "Dataaaaaaa"
  :renderer :opengl
  :setup setup           
  :draw  draw              
  :size [dimensions dimensions])   

(defn -main [] )

