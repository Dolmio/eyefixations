(ns eyefixations.sketcher
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream]])
  (:require [eyefixations.velocityBasedIdentification :as simpleVelBasedIdentification])
  (:require [eyefixations.dispersionBasedIdentification :as dispersionBasedIdentification])
  (:use eyefixations.dataParser)
  (:use eyefixations.sampleFixations))

(defn draw []
  (let [data (state :data)
        sampleFixationData ((:sampleData data))
        veloFixationData ((:velocityData data))
        dispersionFixationData ((:dispersionData data))]

    (background 20)
    (if (:fixation sampleFixationData)
      (do
        (fill 60 60 60)
        (ellipse (:x sampleFixationData) (:y sampleFixationData) 30 30)))

   (if (:fixation veloFixationData)
      (do
        (fill 255 255 255)
        (ellipse (:x veloFixationData) (:y veloFixationData) 20 20)))
    (if (:fixation dispersionFixationData)
      (do
        (fill 160 160 255)
        (ellipse (:x dispersionFixationData) (:y dispersionFixationData) 20 20)))

    ))

(defn setup []
  (smooth)
  (background 0)

  (let [sampleSize 1000
        velocityTreshold 10
        frameRate 10
        rawSampleData (parseRawSampleData sampleSize)
        fixationFrames (sampleFixationFrames
                            (parseRawFixationData)
                            30
                            sampleSize)
        velocityFixationData (simpleVelBasedIdentification/labeledPoints
                               rawSampleData
                               velocityTreshold )

        maxDispersion 15
        minGroupSize 5
        dispersionFixationData (dispersionBasedIdentification/labeledPoints
                                rawSampleData maxDispersion minGroupSize)
        ]
  (frame-rate frameRate)
  ;frame sequences are transformed to streams so when they are accessed from
  ;the draw function they will allways return the value of the next frame
  (set-state! :data {:sampleData (seq->stream fixationFrames)
                     :velocityData (seq->stream velocityFixationData)
                     :dispersionData (seq->stream dispersionFixationData)})))

(defn start-some []
  (defsketch gen-art-24
             :title "Eyefixation viz"
             :setup setup
             :draw draw
             :size [1000 1000])
  )

(defn draw-image[file-name]
  (fn []
    (do
      (fill 60 60 60)
      (ellipse 100 100 30 30)
      (image (state :img) 0 0)
      (let [points (state :points)]
        (doall
          (map #(ellipse (:x %) (:y %) 5 5) points)
          )
        (save file-name)

        )

      )
    )



  )



(defn setup-draw-image [news-id trial-id person-id data-type-to-draw ]
  (fn []
    (let [data (:raw-data (get-datas news-id trial-id person-id))
          ]
      (smooth)
      (background 0)
      (set-state! :img (load-image (get-image-path news-id trial-id))
                  :points data)

      )


  ))



(defn start-draw-image [news-id trial-id person-id]

  (defsketch gen-art-24
             :title "Eyefixation viz"
             :setup (setup-draw-image news-id trial-id person-id :raw-data)
             :draw (draw-image (str "output/" news-id "-" trial-id "-" person-id ".jpg"))
             :size [1000 1000])
  )


(defn save-all-images-with-raw-data []

  (for [news-id (map (partial format "%02d") (range 9 10))
        trial-id (map str (range 7 8))
        person-id (map (partial format "%02d") (range 3 17))]
    (start-draw-image news-id trial-id person-id)

    )

  )