(ns ant-clustering.dataset
  )

;;;;;;;;;;;;;;;;;;;;;
;; Dataset loading ;;
;;;;;;;;;;;;;;;;;;;;;


(def regex-dataset  #"(\-?[\d]+[,.][\d]+)\t(\-?[\d]+[,.][\d]+)\t(\d)")

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
  (map convert-line-to-data lines))

(defn load-dataset [path]
  (mapv :data (lines-to-dataset (file-to-lines path))))

(defn load-dataset-classes [path]
  (mapv :class (lines-to-dataset (file-to-lines path))))
