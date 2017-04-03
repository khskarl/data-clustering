(ns ant-clustering.dataset
  (:require clojure.pprint))

;;;;;;;;;;;;;;;;;;;;;
;; Dataset loading ;;
;;;;;;;;;;;;;;;;;;;;;

;;(def dataset-path "datasets/Square1_DataSet_400itens.txt")
(def dataset-path "datasets/R15.txt")
(def regex-dataset  #"(\-?[\d]+[,.][\d]+)\t(\-?[\d]+[,.][\d]+)\t(\d+)")

(defn file-to-lines [filepath]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (map #(clojure.string/replace % #"," ".")
         (filter #(re-matches regex-dataset %)
                 (apply vector (line-seq rdr))))))

(defn convert-line-to-data [line]  
  (let [captures (rest (re-find regex-dataset line))]
    {:class (read-string (last captures))
     :data (apply vector (butlast (map read-string captures)))}))

(defn lines-to-dataset [lines]
  "Converts lines into a struct-of-arrays of the data"
  (let [ugly-format-data (map convert-line-to-data lines)]
    {:data (mapv :data ugly-format-data)
     :classes (mapv :class ugly-format-data)}))

(defn load-dataset [path] 
  (lines-to-dataset (file-to-lines path)))

(def dataset (load-dataset dataset-path))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;
(defn get-data []
  (:data dataset))

(defn get-classes []
  (:classes dataset))

(defn get-class-indices [class-id]
  (keep-indexed #(when (= class-id %2) %1) (get-classes)))

(defn get-data-from-class [class-id]
  (mapv nth (repeat (get-data)) (get-class-indices class-id)))

;; sketchpad

(def sample (first (get-data-from-class 1)))
(def class-1 (rest (get-data-from-class 1)))
(def class-2 (get-data-from-class 2))
(def class-3 (get-data-from-class 3))
(def class-4 (get-data-from-class 4))

(defn euclidean-distance [a b]  
  (Math/sqrt (reduce + (map #(* %1 %1) (map - a b)))))

(defn data-dissimilarity [data neighbors-data]
  (let [sum (reduce + (map euclidean-distance (repeat data) neighbors-data))]
    (/ sum
       (Math/max (float 1) (float (count neighbors-data)))
       max-distance)))

(defn data-similarity [data neighbors-data]
  (- 1  (* (data-dissimilarity data neighbors-data)
           (data-dissimilarity data neighbors-data) )))

(defn concentration [num-neighbors max-neighbors]
  (float (/ num-neighbors max-neighbors)))

(defn inverse-concentration [num-neighbors max-neighbors]
  (- 1 (concentration num-neighbors max-neighbors)))

;; Pick formula seems to be working well, thank you math! :D
(defn chance-to-pick [data neighbors-data max-neighbors]
  (let [value (Math/abs (+ (data-dissimilarity data neighbors-data)
                           (concentration (count neighbors-data) max-neighbors)
                           (- 1)))]
    (* value 1)))

;; Drop formula seems to work? Thank you random tinkering :D
(defn chance-to-drop [data neighbors-data max-neighbors]
  (- (data-similarity data neighbors-data)
     (inverse-concentration (count neighbors-data) max-neighbors)))

(defn diagnostic-pick [data neighbors-data]
  (println "dissimilarity:" (data-dissimilarity data neighbors-data)
           "\nconcentration:" (concentration (count neighbors-data) 8)
           "\npick chance:  " (chance-to-pick data neighbors-data) 
           "\ndrop chance:  " (chance-to-drop data neighbors-data) ))

(defn diagnostic-drop [data neighbors-data]
  (println "similarity:   " (data-similarity data neighbors-data)
           "\nconcentration:" (concentration (count neighbors-data) 8)
           "\ndrop chance:  " (chance-to-drop data neighbors-data)) )

;; (doseq [n [0 2 4 6 8]]
;;   (println "\n" n)
;;   (diagnostic-pick sample (take n class-1)))

;; (doseq [n [0 2 4 6 8 ]]
;;   (println "\n" n)
;;   (diagnostic-pick sample (take n class-2)))

