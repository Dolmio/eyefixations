(ns eyefixations.core
   (:use clojure.contrib.math))

(defn dataPointsInMinDurationTreshold [fps minDurationTreshold]
  "Calculates the datapoints needed to cover the minimium duration of
  one fixation"
  (int (round (/ minDurationTreshold (/ 1000 fps)))))

(defn dispersionOf [points]
  "D = [max(x) - min(x)] + [max(y) - min(y)]"
  (let [x-positions (map #(:x %) points)
        y-positions (map #(:y %) points)]
    (+ (- (apply max x-positions) (apply min x-positions))
       (- (apply max y-positions) (apply min y-positions)))))

(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn centerOfMass [points]
 (let [x-positions (map #(:x %) points)
        y-positions (map #(:y %) points)]
  {:x (average x-positions) :y (average y-positions)}))

(defn tooMuchDispersion [points maxDispersion]
  (> (dispersionOf points) maxDispersion))

(defn getRawFixationGroups [pointsVector maxDispersion]
  (let [points (into-array pointsVector) initialReduceValue []]
  (areduce points idx ret initialReduceValue
           (let [window (concat (last ret) [(nth points idx)])]
           (if (tooMuchDispersion window maxDispersion)
             (conj ret [(nth points idx)])
             (if (or (empty? ret) (= 1 (count ret)))
               [window]
               (concat (butlast ret) [window])))))))

(defn collapseFixationGroupsByCenterOfMass [fixationGroups]
 (map #(centerOfMass %) fixationGroups))

(defn filterTooShortFixationGroups [fixationGroups minFixationGroupSize]
  (filter #(>= (count %) minFixationGroupSize) fixationGroups))

(defn getFixations [pointsVector maxDispersion minFixationGroupSize]
  (collapseFixationGroupsByCenterOfMass
   (filterTooShortFixationGroups (getRawFixationGroups pointsVector maxDispersion) minFixationGroupSize)))
