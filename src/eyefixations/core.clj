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

(defn mapOverMap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn centerOfMass [points]
  "calculates averages for every key in point"
  (mapOverMap #(/ % (count points)) (apply merge-with + points)))

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
               (concat (drop-last output) [window]))))) initialReduceValue points)))

(defn collapseFixationGroupsByCenterOfMass [fixationGroups]
 (map #(centerOfMass %) fixationGroups))

(defn filterTooShortFixationGroups [fixationGroups minFixationGroupSize]
  (filter #(>= (count %) minFixationGroupSize) fixationGroups))

(defn getFixations [pointsVector maxDispersion minFixationGroupSize]
  (collapseFixationGroupsByCenterOfMass
   (filterTooShortFixationGroups
    (getRawFixationGroups pointsVector maxDispersion) minFixationGroupSize)))

(defn differenceInsideTreshold [a b treshold]
  (<= (abs (- a b)) treshold))

(defn distanceBetween [p1 p2]
  (sqrt (+ (expt (- (:x p1) (:x p2)) 2)
          (expt (- (:y p1) (:y p2)) 2))))

(defn calculateCostBetweenSampleAndActual [sampleFixation actualFixations durationTreshold penaltyForNotFoundingMatch]
  (let [matchedFixations
      (filter #(differenceInsideTreshold (:time sampleFixation) (:time %) durationTreshold) actualFixations)]
    (if (empty? matchedFixations)
      penaltyForNotFoundingMatch
      (distanceBetween sampleFixation (centerOfMass matchedFixations)))))

(defn costFunction [actualFixations sampleFixations durationTreshold penaltyForNotFoundingMatch]
  (apply + (for [sampleFixation sampleFixations] (calculateCostBetweenSampleAndActual sampleFixation actualFixations durationTreshold penaltyForNotFoundingMatch))))


(defn getTrainedFixations [rawEyeData sampleFixations penaltyForNotFoundingMatch]
  (apply min-key :cost (for [durationTreshold (range 1 50 ) maxDispersion (range 1 10) minFixationGroupSize (range 1 10)]
    (let [fixations (getFixations rawEyeData maxDispersion minFixationGroupSize)
          cost (costFunction  fixations sampleFixations durationTreshold penaltyForNotFoundingMatch)]
            {:cost cost :fixations fixations :durationTreshold durationTreshold
             :maxDispersion maxDispersion :minFixationGroupSize minFixationGroupSize}))))
