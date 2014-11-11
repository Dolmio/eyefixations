(ns eyefixations.optimizer
  (:require [eyefixations.velocityBasedIdentification :as simpleVelBasedIdentification])
  (:require [eyefixations.dispersionBasedIdentification :as dispersionBasedIdentification])
  (:use eyefixations.dataParser)
  (:use eyefixations.sampleFixations))

(def frame-rate 30)

(defn difference [frameSetA frameSetB]
 "Counts the difference between two framesets by counting the frames
  where the value whether the frame is a fixation is different than in
  the other frameset"
  (->>
    (map #(not= (:fixation %1) (:fixation %2)) frameSetA frameSetB)
    (filter identity)
    (count))
)


(defn optimizeVelocityBasedIdentification[]
  (let [
        sampleSize 10000
        rawSampleData (parseRawSampleData sampleSize)
        sampleFixations (sampleFixationFrames
                            (parseRawFixationData)
                            frame-rate
                            sampleSize)]

    (apply min-key #(:difference %) (map (fn [treshold] {:difference (difference sampleFixations (simpleVelBasedIdentification/labeledPoints
                                       rawSampleData
                                       treshold)) :treshold treshold}) (range 100)))))


(defn optimizeDispersionBasedIdentification []
  (let [
        sampleSize 2000
        rawSampleData (parseRawSampleData sampleSize)
        sampleFixations (sampleFixationFrames
                            (parseRawFixationData)
                            frame-rate
                            sampleSize)]


    (apply min-key #(:difference %) (for [treshold (range 10 60)
                                          min-size (range 2 10)]
                                      (let [res {:difference (difference sampleFixations (dispersionBasedIdentification/labeledPoints
                                                                                           rawSampleData treshold min-size)) :treshold treshold :min-size min-size}]
                                        (do (println res)
                                            res))
                                      ) )))




(parseRawSampleData 50)
(map (fn[%] {:fixation (:fixation %)}) (sampleFixationFrames (parseRawFixationData) 30 30))

(map (fn[%] {:fixation (:fixation %)}) (simpleVelBasedIdentification/labeledPoints (parseRawSampleData 30)
                    11))
(optimizeVelocityBasedIdentification)

(defn get-longest-fixation-from-sample [data]
  (->> data
       (partition-by #(nil? (:x %)))
       (take-nth 2)
       (apply max-key count)
       (count)
        )
  )

(defn get-fixation-percentage [data]
  (/  (count (filter #(:fixation %) data))
      (count data)))

(defn get-longest-fixation-from-estimate [data]
  (->> data
       (partition-by #(:fixation %))
       (take-nth 2)
       (apply max-key count)
       (count)
       )

  )

(defn average [col]
  (/ (apply + col) (count col)))

(defn get-average-fixation-time [data]
  (->> data
       (partition-by #(:fixation %))
       (take-nth 2)
       (map count)
       (average)
       (#(/ % frame-rate))
       (double)

       ))

(defn sampleFixations [sample-size]
  (sampleFixationFrames
    (parseRawFixationData)
    frame-rate
    sample-size))

(defn velocity-fixation-data [sample-size treshold]
  (simpleVelBasedIdentification/labeledPoints
    (parseRawSampleData sample-size)
    treshold))