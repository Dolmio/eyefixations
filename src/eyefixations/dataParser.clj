(ns eyefixations.dataParser
  (:use [clojure.string :only (split)]))

(defn lazyFileLines [file]
  (letfn [(helper [rdr]
                  (lazy-seq
                    (if-let [line (.readLine rdr)]
                      (cons line (helper rdr))
                      (do (.close rdr) nil))))]
         (helper (clojure.java.io/reader file))))

(defn parseRows[fileName]
  (map #(split % #"\t") (lazyFileLines fileName)))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn parseData [filename wantedColumnMappings & options]
  (let [raw_rows (parseRows filename)
      frameCount (first options)
      rows (if frameCount (take (inc frameCount) raw_rows) raw_rows)
      colNames (first rows)]

   (map
    ( fn[row] (apply hash-map
                     (flatten(map ( fn[k](list (get wantedColumnMappings k)
                                               (parse-int (nth row (.indexOf colNames k)))))
                (keys wantedColumnMappings)))))
    (rest rows))))

(defn parseRawSampleData [& options]
  (let [columnMappings {"B POR X [px]" :x
                        "B POR Y [px]" :y}
        frameLimit (first options)]
    (parseData "resources/sample1.txt" columnMappings frameLimit)))


