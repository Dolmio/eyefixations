(ns eyefixations.math
   (:use clojure.contrib.math))

(defn distanceBetween [p1 p2]
  (sqrt (+ (expt (- (:x p1) (:x p2)) 2)
          (expt (- (:y p1) (:y p2)) 2))))

(defn mapOverMap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn centerOfMass [points]
  "calculates averages for every key in point"
  (mapOverMap #(/ % (count points)) (apply merge-with + points)))

