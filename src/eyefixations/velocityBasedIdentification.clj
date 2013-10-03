(ns eyefixations.velocityBasedIdentification
   (:use eyefixations.math))

"Calculate point-to-point velocities (distances between consecutive points)
for each point in the dataset.
Partition dataset by velocity treshold and filter groups with too high
velocity out.
Map each fixation group to a fixation at the centroid of its points
Return fixations"

(defn pointPairsBelowVelocityTreshold [pointPairs velocityTreshold]
  (filter #(< (:distance (first %)) velocityTreshold)
   (partition-by #(> (:distance %) velocityTreshold) pointPairs)))

(defn getDistancesBetweenPoints [pointsVector]
  (let [pointPairs (partition 2 1 pointsVector)]
  (map (fn [%] (let [p1 (first %) p2 (second %)]
                 {:p1 p1  :p2 p2 :distance (distanceBetween p1 p2)})) pointPairs)))

(defn centroid [pointPairs]
  (centerOfMass (conj
                   (map #(:p1 %) pointPairs)
                   (:p2 (last pointPairs)))))

(defn getFixations [pointsVector velocityTreshold]
  (map #(centroid %) (pointPairsBelowVelocityTreshold
                           (getDistancesBetweenPoints pointsVector)
                            velocityTreshold)))

