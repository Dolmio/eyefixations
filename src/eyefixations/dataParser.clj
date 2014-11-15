(ns eyefixations.dataParser
  (:use [clojure.string :only (split)]))

(defn lazyFileLines [file]
  (letfn [(helper [rdr]
                  (lazy-seq
                    (if-let [line (.readLine rdr)]
                      (cons line (helper rdr))
                      (do (.close rdr) nil))))]
         (helper (clojure.java.io/reader file))))

(defn parseRows[fileName separator]
  (map #(split % separator) (lazyFileLines fileName)))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn parseData [filename wantedColumnMappings & options]
  (let [raw_rows (parseRows filename #"\t")
      frameCount (first options)
      rows (if frameCount (take (inc frameCount) raw_rows) raw_rows)
      colNames (first rows)]

   (map
    ( fn[row] (apply hash-map
                     (flatten(map ( fn[k](list (get wantedColumnMappings k)
                                               (parse-int (nth row (.indexOf colNames k)))))
                (keys wantedColumnMappings)))))
    (rest rows))))

(defn map-map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn parseRawData [filename]
  (let [raw_rows (parseRows filename #"\s")]
    (->> raw_rows
         (map #(hash-map :x (second %) :y (nth % 2)))
         (map #(map-map-vals parse-int %))
         )


    )
 )

(defn get-alphabet-from [news-id]
  (->> news-id
       (parse-int)
       (dec)
       (nth (map char  (range 97 123)))
       (str))

  )

(def raw-data-folder "resources/splitted_raw_data/")
(defn raw-data-path [news-id trial-id person-id]
  (str raw-data-folder (get-alphabet-from news-id) trial-id "_h.jpg_" person-id "H.txt"))

(defn parseRawFixationData [filename]
  (let [raw_rows (parseRows filename #"\,")]
    (->> raw_rows
         (map #(hash-map :startTime (second %)
                         :endTime (nth % 2)
                         :x (nth % 3)
                         :y (nth % 4)))
         (map #(map-map-vals parse-int %))
         )))



(def fixation-data-folder "resources/fixationdata/")
(defn fixation-data-path [news-id trial-id person-id]
  (str fixation-data-folder (get-alphabet-from news-id) trial-id "_h.jpg_" person-id "H.txt"))


(defn parseRawSampleData [& options]
  (let [columnMappings {"B POR X [px]" :x
                        "B POR Y [px]" :y}
        frameLimit (first options)]
    (parseData "resources/sample1.txt" columnMappings frameLimit)))

(defn get-datas [news-id trial-id person-id]
  {:raw-data (parseRawData (raw-data-path news-id trial-id person-id))
   :fixation-data (parseRawFixationData (fixation-data-path news-id trial-id person-id))})



(defn get-image-path [news-id trial-id]
  (str "resources/images/" news-id "/" (get-alphabet-from news-id) trial-id "_h.jpg")
  )