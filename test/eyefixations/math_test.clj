(ns eyefixations.math-test
  (:require [clojure.test :refer :all])
  (:use eyefixations.math))

(deftest distanceBetweenTest
    (is (= 5 (distanceBetween {:x 0 :y 0} {:x 3 :y 4}))))

(deftest centerOfMassTest
    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 100 :y 50 :time 0}]
    (is (= {:x 50 :y 25 :time 0} (centerOfMass [p1 p2]))))

    (let [p1 {:x 0 :y 0 :time 0} p2 {:x 0 :y -100 :time 0}]
    (is (= {:x 0 :y -50 :time 0} (centerOfMass [p1 p2])))))
