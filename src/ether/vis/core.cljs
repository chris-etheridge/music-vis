(ns ether.vis.core
  (:require [rum.core :as rum]))

(defn attach-audio-analyzer! [ele-id]
  (let [ctx     (js/AudioContext.)
        element (js/document.getElementById ele-id)
        src     (.createMediaElementSource ctx element)
        ana     (.createAnalyser ctx)]
    (set! js/window.ana ana)
    (set! js/window.actx ctx)
    (set! js/window.src src)
    (set! (.-fftSize ana) 2048)
    (.connect src ana)
    (.connect ana (.-destination ctx))
    {:audio/context ctx :audio/src src :audio/analyser ana}))

;; https://github.com/audacity/audacity/blob/master/src/BlockFile.cpp#L203
(defn calc-summary-buffer [buffer length]
  (let [sum-length (/ (+ length 255) 256)
        result (js/Array. length)]
    (loop [iter sum-length]
      (if (pos? (dec iter))
        (let [*min   (atom (aget buffer (* iter 256)))
              *max   (atom (aget buffer (* iter 256)))
              min    @*min
              max    @*max
              sq     (* min max)
              jcount 256
              jcount (if (< jcount (- length (* iter 256)))
                       (- length (* iter 256))
                       jcount)
              sm     (loop [jiter jcount
                            sumsq sq]
                       (if (pos? (dec jiter))
                         (let [f1 (aget buffer (+ (* iter 256) jiter))]
                           (if (< f1 min)
                             (reset! *min f1)
                             (when (< f1 max)
                               (reset! *max f1)))
                           (recur (dec jiter) (+ (* f1 f1) sumsq)))
                         sumsq))
              rms    (Math/sqrt (/ sm jcount))]
          (aset result (* iter 3) @*min)
          (aset result (+ (* iter 3) 1) @*max)
          (aset result (+ (* iter 3) 2) rms)
          (recur (dec iter)))
        result))))

(defn setup-canvas! []
  (let [cv (js/document.getElementById "draw-area")
        ctx (.getContext cv "2d")]
    (set! (.-fillStyle ctx) "rgb(200, 200, 200)")
    (.fillRect ctx 0 0 1200 800)
    (set! (.-lineWidth ctx) 2)
    (set! (.-strokeStyle ctx) "rgb(0, 0, 0)")
    ctx))

(defn draw-buffer-lines [ctx buffer n w]
  (loop [i 0
         x 0]
    (if-not (= i (.-length buffer))
      (let [v (/ (aget buffer i) 4)
            y (+ 450 (* v 450))]
        (if (= i 0)
          (.moveTo ctx x y)
          (.lineTo ctx x y))
        (recur (inc i) (+ x w)))
      nil)))

(defn ^:export draw! [ctx buffer]
  (when buffer
    (let [n (.-length buffer)
          w (/ (* 1280 1) n)]
      (.fillRect ctx 0 0 1200 800)
      (.beginPath ctx)
      (draw-buffer-lines ctx buffer n w)
      (.stroke ctx)
      (prn :d)
      )))

(defn reload! []
  (prn "reloading ..."))

(defn load-sound [*ref audio]
  (let [end "/samples/carbs.mp3"
        req (js/XMLHttpRequest.)]
    (.open req "GET" end true)
    (set! (.-responseType req) "arraybuffer")
    (set! (.-onload req)
          (fn []
            (.decodeAudioData (:audio/context audio)
                              (.-response req)
                              (fn [b]
                                (reset! *ref b)))))
    (.send req)))

(defn start! []
  (let [ctx   (setup-canvas!)
        audio (attach-audio-analyzer! "carbs")
        *r (atom nil)
        _ (add-watch *r ::w (fn [& _]
                              (js/console.log @*r)
                              (set! js/window.ab @*r)
                              (js/console.log (.getChannelData @*r 0))
                              (draw! ctx (.getChannelData @*r 0))))]
    (load-sound *r audio)))
