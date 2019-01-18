(ns ^:figwheel-hooks paren.hero
  (:require
   [goog.dom :as gdom]
   [goog.events :as gevents]
   [goog.events.KeyHandler]
   [oops.core :refer [ocall oget oset!]]
   [clojure.string :as string]))

;; define your app data so that it doesn't get over-written on reload
(def init-state
  {:elapsed-time 0
   :delta 0
   :screen-size {:cw 0
                 :ch 0}
   :caret {:x 0
           :y 0}
   :forms [{:progress 0.5
            :slot 0
            :bits [{:text "("}
                   {:text " + 1 2 "
                    :hot? true}
                   {:text ")"}
                   {:text " 3"}]
            :success {:text "( + 1 2 3 )"}
            :chord #{17 65}
            :velocity 0.06}]})

(defn chord-pressed
  "Returns the time the chord was completed"
  [chord ks]
  (when (= (set (keys ks)) chord)
    (apply max (vals ks))))

(def game-state
  (atom init-state))

(defn init! []
  (reset! game-state init-state))

(def canvas
  (gdom/getElement "canvas"))

(def cx
  (ocall canvas "getContext" "2d"))

(defn puts
  [s]
  (let [{:keys [size]} @game-state
        {:keys [cw ch]} size
        lines (reverse (string/split s #"\n"))]
    (oset! cx "fillStyle" "#FFFFFF")
    (oset! cx "font"  "10px Menlo")
    (loop [[l & more] lines
           y 20]
      (ocall cx "fillText" l 0 (- ch y))
      (when more
        (recur more
               (+ y 12))))))

(defn get-size
  []
  (let [bounding-rect (ocall canvas "getBoundingClientRect")]
    {:cw (oget bounding-rect "width")
     :ch (oget bounding-rect "height")}))

(defn update-caret
  [game-state]
  (let [{:keys [size]} game-state
        {:keys [cw ch]} size]
    (assoc game-state :caret {:x (* cw 0.25)
                              :y 0
                              :w 2
                              :h ch})))


(defn intersects?
  [bit caret]
  (let [{cx :x cw :w} caret
        {bx :x bw :w} bit]
    (and (<= bx cx)
         (<= (+ cx cw) (+ bx bw)))))

(defn update-form
  [form]
  (oset! cx "font"  "80px Menlo")
  (let [{:keys [bits
                progress
                slot
                chord
                success
                win?]} form
        {:keys [size
                caret
                elapsed-time
                keys]} @game-state
        {:keys [cw ch]} size
        x (- cw (* progress cw))
        y (+ 100 (* slot (/ ch 10)))
        form (assoc form :x x,
                    :y y,
                    :active? false
                    :color "#FFFFFF")]
    (loop [i 0
           xacc x
           form form
           bits bits]
      (if-not (< i (count bits))
        (assoc form :bits bits)
        (let [bit (nth bits i)
              {:keys [text]} bit
              w (oget (ocall cx "measureText" text) "width")
              bit (assoc bit :y y :w w :x xacc)
              active? (and (:hot? bit)
                           (intersects? bit caret))
              active-from (or (:active-from form) elapsed-time)
              win? (or win? (> (chord-pressed chord keys) active-from))
              form (if active?
                     (assoc form
                            :color "#FF0000"
                            :active-from active-from)
                     form)
              bit (assoc bit :active? active?)]
          (if win?
            (assoc form
                   :bits [(assoc success :x x :y y)]
                   :win? true
                   :color "#00FF00") 
            (recur (inc i)
                   (+ xacc w)
                   (assoc form
                          :active? (or (:active? form) active?))
                   (assoc bits i bit))))))))

(defn key-up
  [game-state key-code]
  (update game-state :keys dissoc key-code))

(defn key-down
  [game-state key-code]
  (assoc-in game-state [:keys key-code] (:elapsed-time game-state)))

(defn draw-bit [bit]
  (let [{:keys [text x y]} bit]
    (ocall cx "fillText" text x y )))

(defn draw-form
  [form]
  (let [{:keys [progress slot color bits]} form
        {:keys [size]} @game-state
        {:keys [cw ch]} size]
    (oset! cx "fillStyle" color)
    (oset! cx "font"  "80px Menlo")
    (run! draw-bit bits)))

(defn draw-caret
  [game-state]
  (let [{:keys [caret]} game-state
        {:keys [x y w h]} caret]
    (oset! cx "fillStyle" "#FFFFFF")
    (ocall cx "fillRect" x y w h)))

(defn draw [game-state]
  (let [{:keys [last-time forms size]} game-state
        {:keys [cw ch]} size]
    (draw-caret game-state)
    (run! draw-form forms)
    (puts (with-out-str (cljs.pprint/pprint game-state)))))

(defn update-forms [game-state]
  (let [{:keys [delta]} game-state]
    (update game-state :forms
            (fn [forms]
              (mapv (fn [f]
                     (let [{:keys [progress velocity]} f
                           new-progress (+ progress (* delta velocity))]
                       (-> (assoc f :progress new-progress)
                           (update-form)))) forms)))))

(defonce last-time (atom nil))

(defn envstate
  []
  (let [current-time (.getTime (js/Date.))
        size (get-size)
        t (or @last-time current-time)
        delta (/ (- current-time t) 1000)]
    {:size size
     :current-time current-time
     :delta delta}))

(defn update-game
  [game-state envstate]
  (let [{:keys [elapsed-time]} game-state
        {:keys [delta size]} envstate]
    (-> (assoc
          game-state
          :size size
          :elapsed-time (+ elapsed-time delta)
          :delta delta)
        (update-caret)
        (update-forms))))

(defn clear-screen
  [cw ch]
  (ocall cx "clearRect" 0 0 cw ch)
  (oset! cx "fillStyle" "#333333")
  (ocall cx "fillRect" 0 0 cw ch))

(defn game-loop []
  (js/requestAnimationFrame game-loop)

  (let [{:keys [current-time
                size]
         :as envstate} (envstate)
        {:keys [cw ch]} size]
    (oset! canvas "width" cw)
    (oset! canvas "height" ch)
    (clear-screen cw ch)
    (try
      (let [new-state (swap! game-state update-game envstate)]
        (draw new-state))
      (catch :default e
        (puts e)))

    (reset! last-time current-time)))

(defn key-up-handler
  [event]
  (let [key-code (oget event "keyCode")]
    (swap! game-state key-up key-code)))

(defn key-down-handler
  [event]
  (let [key-code (oget event "keyCode")]
    (swap! game-state key-down key-code)))

(defn init-input
  []
  (oset! js/document "onkeydown" key-down-handler)
  (oset! js/document "onkeyup" key-up-handler))

(defonce _ (do
             (init-input)
             (game-loop)))


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
