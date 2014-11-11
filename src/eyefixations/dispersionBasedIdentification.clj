(ns eyefixations.dispersionBasedIdentification
   (:use clojure.contrib.math)
   (:use eyefixations.math)
  (:use eyefixations.dataParser))

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

(defn tooMuchDispersion [points maxDispersion]
  (> (dispersionOf points) maxDispersion))

(defn notFailedMeasurement [point]
  (not (= 0 (:x point) (:y point)) ))

(defn getRawFixationGroups [points maxDispersion]
  (let [initialReduceValue []]
  (reduce (fn [output point]
    (let [window (concat (last output) [point])]
           (if (tooMuchDispersion (filter notFailedMeasurement window) maxDispersion)
             (conj (vec output) [point])
             (if (or (empty? output) (= 1 (count output)))
               [window]
               (concat (drop-last output) [window]))))) initialReduceValue points)))

(defn collapseFixationGroupsByCenterOfMass [fixationGroups]
 (map #(centerOfMass %) fixationGroups))

(defn filterTooShortFixationGroups [fixationGroups minFixationGroupSize]
  (filter #(>= (count %) minFixationGroupSize) fixationGroups))

(defn getFixations [pointsVector maxDispersion minFixationGroupSize]
  (-> pointsVector
      (getRawFixationGroups maxDispersion)
      (filterTooShortFixationGroups minFixationGroupSize)
      (collapseFixationGroupsByCenterOfMass)))

(defn labeledPoints [pointsVector maxDispersion minFixationGroupSize]
   (flatten (map (fn[group]
                  (map #(merge % {:fixation (>= (count group) minFixationGroupSize)})
                 group))
            (getRawFixationGroups pointsVector maxDispersion))))

(defn differenceInsideTreshold [a b treshold]
  (<= (abs (- a b)) treshold))

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
