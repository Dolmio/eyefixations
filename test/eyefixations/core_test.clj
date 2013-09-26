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
    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 100 :y 50 :time 0}]
    (is (= {:x 50 :y 25 :time 0} (centerOfMass [p1 p2]))))

    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 0 :y -100 :time 0}]
    (is (= {:x 0 :y -50 :time 0} (centerOfMass [p1 p2])))))

  (deftest collapseFixationGroupsByCenterOfMassTest
    (let [g1 [{:x 0 :y 0 :time 0} {:x 100 :y 50 :time 0}] g2 [{:x 100 :y 100 :time 0} {:x 200 :y 200 :time 0}]]
      (is (= [{:x 50 :y 25 :time 0} {:x 150 :y 150 :time 0}] (collapseFixationGroupsByCenterOfMass [g1 g2])))))


  (deftest getFixationsTest
    "two fixation groups should be found and p6 should be filtered out"
    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 120 :y 60 :time 0} p3 {:x 0 :y 0 :time 0} p4 {:x 500 :y 500 :time 0}
          p5 {:x 600 :y 600 :time 0} p6 {:x 1000 :y 1000 :time 0} minGroupSize 2 maxDispersion 200]
      (is (= [{:x 40 :y 20 :time 0} {:x 550 :y 550 :time 0}] (getFixations [p1 p2 p3 p4 p5 p6] maxDispersion minGroupSize))))
    "no Fixation groups should be found"
    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 120 :y 60 :time 0} p3 {:x 0 :y 0 :time 0} minGroupSize 4 maxDispersion 1000]
      (is (= [] (getFixations [p1 p2 p3] maxDispersion minGroupSize))))
    "no Fixation groups should be found because of small maxDispersion"
    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 120 :y 60 :time 0} p3 {:x 0 :y 0 :time 0} minGroupSize 2 maxDispersion 1]
      (is (= [] (getFixations [p1 p2 p3] maxDispersion minGroupSize)))))

  (deftest differenceInsideTresholdTest
    (is (= true (differenceInsideTreshold 10 5 6)))
    (is (= true (differenceInsideTreshold 10 20 10)))
    (is (= false (differenceInsideTreshold 10 8 1))))

  (deftest distanceBetweenTest
    (is (= 5 (distanceBetween {:x 0 :y 0} {:x 3 :y 4}))))

  (deftest calculateCostBetweenSampleAndActualTest
    (let [sampleFixation {:time 10 :x 0 :y 0} actualFixations [{:time 10 :x 3 :y 4}]
          durationTreshold 0 penaltyForNotFoundingMatch 0]
    (is (= 5 (calculateCostBetweenSampleAndActual sampleFixation
                                                        actualFixations
                                                        durationTreshold
                                                        penaltyForNotFoundingMatch)))))

  (deftest getTrainedFixationsTest
    (let [rawEyeData [{:time 10 :x 0 :y 0} {:time 20 :x 300 :y 300}]
          sampleFixations [{:time 10 :x 0 :y 0} {:time 20 :x 300 :y 300}]
          penaltyForNotFoundingMatch 100]
      (is (= 0 (:cost (time (getTrainedFixations rawEyeData sampleFixations penaltyForNotFoundingMatch))))))

    "there is one sample fixation that is not matched"
    (let [rawEyeData [{:time 10 :x 0 :y 0} {:time 20 :x 300 :y 300}]
          sampleFixations [{:time 10 :x 0 :y 0} {:time 20 :x 300 :y 300} {:time 30 :x 1000 :y 1000}]
          penaltyForNotFoundingMatch 100]
      (is (= 100 (:cost (time (getTrainedFixations rawEyeData sampleFixations penaltyForNotFoundingMatch)))))))


