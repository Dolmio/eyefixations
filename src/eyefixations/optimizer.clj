(ns eyefixations.optimizer
  (:require [eyefixations.velocityBasedIdentification :as simpleVelBasedIdentification])
  (:require [eyefixations.dispersionBasedIdentification :as dispersionBasedIdentification])
  (:use eyefixations.dataParser)
  (:use eyefixations.sampleFixations))


(defn difference [frameSetA frameSetB]
 "Counts the difference between two framesets by counting the frames
  where the value whether the frame is a fixation is different than in
  the other frameset"
 (apply + (map (fn [setA setB]
        (if (= (:fixation setA) (:fixation setB))
          0
          1)) frameSetA frameSetB)))


(defn optimizeVelocityBasedIdentification[]
  (let [
        sampleSize 10000
        rawSampleData (parseRawSampleData sampleSize)
        velocityTreshold 10
        sampleFixations (sampleFixationFrames
                            (parseRawFixationData)
                            30
                            sampleSize)
        velocityFixationData (simpleVelBasedIdentification/labeledPoints
                               rawSampleData
                               velocityTreshold )]

    (apply min-key #(:difference %) (map (fn [treshold] {:difference (difference sampleFixations (simpleVelBasedIdentification/labeledPoints
                                       rawSampleData
                                       treshold)) :treshold treshold}) (range 50)))))


(defn optimizeDispersionBasedIdentification []
  (let [
        sampleSize 20
        rawSampleData (parseRawSampleData sampleSize)
        sampleFixations (sampleFixationFrames
                            (parseRawFixationData)
                            30
                            sampleSize)]

    (apply min-key #(:difference %) (map (fn [treshold] {:difference (difference sampleFixations (dispersionBasedIdentification/labeledPoints
                                rawSampleData treshold 3)) :treshold treshold}) (range 100)))))




(parseRawSampleData 50)
(map (fn[%] {:fixation (:fixation %)}) (sampleFixationFrames (parseRawFixationData) 30 30))

(map (fn[%] {:fixation (:fixation %)}) (simpleVelBasedIdentification/labeledPoints (parseRawSampleData 30)
                    11))
(optimizeVelocityBasedIdentification)

