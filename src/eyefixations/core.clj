(ns eyefixations.core
  (:require [clojure.string :as cstr])
  (:import java.util.Date)
  (:import java.io.File)
  (:use clojure-csv.core)
  (:use eyefixations.velocityBasedIdentification))

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

(defn parseRawSampleData [filename & options]
(let [raw_rows (fetch-csv-data filename)
      frameCount (first options)
      rows (if frameCount (take (+ 1 frameCount) raw_rows) raw_rows)
      colNames (first rows)
      xIdx (.indexOf colNames "B POR X [px]")
      yIdx (.indexOf colNames "B POR Y [px]")
      timeIdx (.indexOf colNames "Frame")]

  (map (fn[%] {:x (parse-int (nth % xIdx))
               :y (parse-int (nth % yIdx))
               :time 1})(rest rows))))


