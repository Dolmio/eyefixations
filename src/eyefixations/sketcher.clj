(ns eyefixations.sketcher
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream]])
  (:require [eyefixations.velocityBasedIdentification :as simpleVelBasedIdentification])
  (:use eyefixations.dataParser)
  (:use eyefixations.sampleFixations))

(defn draw []
  (let [data (state :data)
        sampleFixationData ((:sampleData data))
        veloFixationData ((:velocityData data))]
    (background 180)
    (if (:fixation sampleFixationData)
      (do
        (fill 60 60 60)
        (ellipse (:x sampleFixationData) (:y sampleFixationData) 30 30)))

    (if (:fixation veloFixationData)
      (do
        (fill 255 255 255)
        (ellipse (:x veloFixationData) (:y veloFixationData) 20 20)))
    ))

(defn setup []
  (smooth)
  (background 0)

  (let [sampleSize 1000
        velocityTreshold 10
        frameRate 10
        fixationFrames (sampleFixationFrames
                            (parseRawFixationData)
                            30
                            sampleSize)
        velocityFixationData (simpleVelBasedIdentification/labeledPoints
                              (parseRawSampleData sampleSize)
                               velocityTreshold )]
  (frame-rate frameRate)
  ;frame sequences are transformed to streams so when they are accessed from
  ;the draw function they will allways return the value of the next frame
  (set-state! :data {:sampleData (seq->stream fixationFrames)
                     :velocityData (seq->stream velocityFixationData)})))

(defsketch gen-art-24
  :title "Eyefixation viz"
  :setup setup
  :draw draw
  :size [1000 1000])
