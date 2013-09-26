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

(defn getRawFixationGroups [points maxDispersion]
  (let [initialReduceValue []]
  (reduce (fn [output point] 
    (let [window (concat (last output) [point])]
           (if (tooMuchDispersion window maxDispersion)
             (conj output [point])
             (if (or (empty? output) (= 1 (count output)))
               [window]
               (concat (butlast output) [window]))))) initialReduceValue points)))

(defn collapseFixationGroupsByCenterOfMass [fixationGroups]
 (map #(centerOfMass %) fixationGroups))

(defn filterTooShortFixationGroups [fixationGroups minFixationGroupSize]
  (filter #(>= (count %) minFixationGroupSize) fixationGroups))

(defn getFixations [pointsVector maxDispersion minFixationGroupSize]
  (collapseFixationGroupsByCenterOfMass
   (filterTooShortFixationGroups
    (getRawFixationGroups pointsVector maxDispersion) minFixationGroupSize)))
