(ns eyefixations.core-test
  (:require [clojure.test :refer :all])
  (:use eyefixations.core))

(deftest dataPointsInMinimiumDurationTreshold
    (is (=  5 (let [fps 30 minDurationTreshold 150]
                (dataPointsInMinDurationTreshold fps minDurationTreshold))))
    (is (=  4 (let [fps 29 minDurationTreshold 150]
                (dataPointsInMinDurationTreshold fps minDurationTreshold)))))

 (deftest dispersionOfPoints
   (let [p1 {:x 0 :y 0} p2 {:x -100 :y 50} p3 {:x 100 :y 100}]
    (is (= 300 (dispersionOf [p1 p2 p3])))
    )

   (let [p1 {:x 0 :y 0} p2 {:x 0 :y 0} p3 {:x 0 :y 0}]
    (is (= 0 (dispersionOf [p1 p2 p3])))
    )
   )

  (deftest centerOfMassTest
    (let [p1 {:x 0 :y 0} p2 {:x 100 :y 50}]
    (is (= {:x 50 :y 25} (centerOfMass [p1 p2]))))

    (let [p1 {:x 0 :y 0} p2 {:x 0 :y -100}]
    (is (= {:x 0 :y -50} (centerOfMass [p1 p2])))))

  (deftest collapseFixationGroupsByCenterOfMassTest
    (let [g1 [{:x 0 :y 0} {:x 100 :y 50}] g2 [{:x 100 :y 100} {:x 200 :y 200}]]
      (is (= [{:x 50 :y 25} {:x 150 :y 150}] (collapseFixationGroupsByCenterOfMass [g1 g2])))))


  (deftest getFixationsTest
    "two fixation groups should be found and p6 should be filtered out"
    (let [p1 {:x 0 :y 0} p2 {:x 120 :y 60} p3 {:x 0 :y 0} p4 {:x 500 :y 500}
          p5 {:x 600 :y 600} p6 {:x 1000 :y 1000} minGroupSize 2 maxDispersion 200]
      (is (= [{:x 40 :y 20} {:x 550 :y 550}] (getFixations [p1 p2 p3 p4 p5 p6] maxDispersion minGroupSize))))
    "no Fixation groups should be found"
    (let [p1 {:x 0 :y 0} p2 {:x 120 :y 60} p3 {:x 0 :y 0} minGroupSize 4 maxDispersion 1000]
      (is (= [] (getFixations [p1 p2 p3] maxDispersion minGroupSize))))
    "no Fixation groups should be found because of small maxDispersion"
    (let [p1 {:x 0 :y 0} p2 {:x 120 :y 60} p3 {:x 0 :y 0} minGroupSize 2 maxDispersion 1]
      (is (= [] (getFixations [p1 p2 p3] maxDispersion minGroupSize)))))

