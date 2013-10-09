(ns eyefixations.sketcher
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]])
  (:require [eyefixations.velocityBasedIdentification :as simpleVelBasedIdentification])
  (:use eyefixations.core))

(defn draw []
  (let [frameData ((state :framedata))]
    (background 180)
    (if (:fixation frameData)
      (ellipse (:x frameData) (:y frameData) 20 20))))


(defn setup []
  (smooth)
  (background 0)
  (frame-rate 30)
  (let [sampleSize 5000
        velocityTreshold 20
        data (simpleVelBasedIdentification/labeledPoints
              (parseRawSampleData "resources/sample1.txt" sampleSize)
              velocityTreshold )]

  (set-state! :framedata (seq->stream data))))

(defsketch gen-art-24
  :title "Animated Fluffy Clouds"
  :setup setup
  :draw draw
  :size [1000 1000])
