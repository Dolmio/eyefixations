(ns eyefixations.velocityBasedIdentification-test
  (:require [clojure.test :refer :all])
  (:use eyefixations.velocityBasedIdentification))

(deftest getDistancesBetweenPointsTest
  (let [p1 {:x 0 :y 0} p2 {:x 100 :y 0} p3 {:x 100 :y 50}]
  (is (=[{:p1 p1 :p2 p2 :distance 100}
         {:p1 p2 :p2 p3  :distance 50}]
       (getDistancesBetweenPoints [p1 p2 p3])))))

(deftest PointPairsBelowVelocitytresholdTest
  (let [p1 {:distance 100} p2 {:distance 150} p3 {:distance 250} p4 {:distance 0}]

    (is (= [[p1 p2] [p4]] (pointPairsBelowVelocityTreshold [p1 p2 p3 p4] 200)))))

(deftest getFixationsTest
  (let [p1 {:x 0 :y 0}
        p2 {:x 100 :y 80}
        p3 {:x 1000 :y 0}
        p4 {:x 0 :y 0}
        p5 {:x 10 :y 20}
        treshold 300]
    (is (= [{:x 50 :y 40} {:x 5 :y 10}] (getFixations [p1 p2 p3 p4 p5] treshold)))))
