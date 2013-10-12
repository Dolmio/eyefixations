(ns eyefixations.dataParser
  (:require [clojure.string :as cstr])
  (:import java.util.Date)
  (:import java.io.File)
  (:use clojure-csv.core))

(defn open-file [file-name]
  (let [file-data (try
               (slurp file-name)
               (catch Exception e (println (.getMessage e))))]
  file-data))

(defn ret-csv-data [fnam]
  (let [csv-file (open-file fnam)
      inter-csv-data (if-not (nil? csv-file)
                       (parse-csv csv-file :delimiter \tab)
                        nil)

      csv-data
        (vec (filter #(and pos? (count %)
           (not (nil? (rest %)))) inter-csv-data))]

    (if-not (empty? csv-data)
      (pop csv-data)
       nil)))

(defn fetch-csv-data [csv-file]
        (let [csv-data (ret-csv-data csv-file)]
            csv-data))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn parseData [filename wantedColumnMappings & options]
  (let [raw_rows (fetch-csv-data filename)
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




