(ns eyefixations.sampleFixations
 (:use eyefixations.dataParser)
 (:use clojure.contrib.math))

(defn framesInRange [rangeMap frameRate]
  (let [duration (- (:endTime rangeMap) (:startTime rangeMap))]
    (round (/ duration (float (/ 1000 frameRate))))))

(defn getRangesBetweenFixations [fixationData]
  (map (fn[pair] {:startTime (:endTime (first pair))
                  :endTime (:startTime (second pair))})
       (partition 2 1 fixationData)))


(defn sampleFixationFrames [fixationData frameRate & options]
  "assumes that there is always fixation at startTime 0"
  (let [frameCount (first options)
        frames (flatten (map-indexed (fn [index frameRange]
                                     (repeat (framesInRange frameRange frameRate)
                                             {:x (:x frameRange)
                                              :y (:y frameRange)
                                              :fixation (even? index)}))
              (interleave fixationData (getRangesBetweenFixations fixationData))))]
      (if frameCount
        (take frameCount frames)
        frames)))

(defn parseRawFixationData-old []
  (let [columnMappings {"Position X" :x
                        "Position Y" :y
                        "Fixation Start [ms]" :startTime
                        "Fixation End [ms]" :endTime}]
    (parseData "resources/sample1_fixations.txt" columnMappings)))

