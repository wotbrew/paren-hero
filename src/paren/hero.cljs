(ns ^:figwheel-hooks paren.hero
  (:require
    [goog.dom :as gdom]
    [goog.events.KeyHandler]
    [oops.core :refer [ocall oget oset!]]
    [clojure.string :as string]
    [paren.keyboard :as keyboard]
    [paren.paredit :as paredit]
    [clojure.set :as set]))

;; define your app data so that it doesn't get over-written on reload
(def init-state
  {:elapsed-time 0
   :delta 0
   :screen-size {:cw 0
                 :ch 0}
   :caret {:x 0
           :y 0}
   :score 0
   :forms []})

(def ^:dynamic *score* nil)

(def slots (set (range 10)))

(defn free-slot
  [game-state]
  (let [{:keys [forms]} game-state
        in-use (set (map :slot forms))]
    (if (< (count in-use) 3)
      (first (shuffle (set/difference slots in-use))))))

(def form-lib
  [{:chord #{:s :ctrl-left}
    :bits [{:text "{"}
           {:text " [k v] "
            :hot? true}
           {:text "}"}]
    :value 10
    :success {:text "{ k v }"}}

   {:chord #{:d :ctrl-left}
    :bits [{:text "("}
           {:text " + 1 2 "
            :hot? true}
           {:text ")"}
           {:text " 3"}]
    :value 10
    :success {:text "(+ 1 2 3)"}}

   {:chord #{:a :ctrl-left}
    :bits [{:text "+ ("}
           {:text " 1 2 "
            :hot? true}
           {:text ")"}]
    :value 10
    :success {:text "(+ 1 2 3)"}}

   {:chord #{:s :ctrl-left}
    :bits [{:text "[["}
           {:text " free-me! "
            :hot? true}
           {:text "]]"}]

    :step {:chord #{:s :ctrl-left}
           :value 20
           :bits [{:text "["}
                  {:text " once-more! "
                   :hot? true}
                  {:text "]"}]
           :success {:text "thank-you!"}}}


   {:chord #{:d :ctrl-left}
    :bits [{:text "["}
           {:text " a b c "
            :hot? true}
           {:text "] d e"}]
    :step {:chord #{:d :ctrl-left}
           :value 20
           :bits [{:text "["}
                  {:text " a b c d"
                   :hot? true}
                  {:text "] e"}]
           :success {:text "[a b c d e]"}}}])

(defn generate-form
  [game-state]
  (when-some [slot (free-slot game-state)]
    (merge
      {:slot slot
       :value 10
       :progress 0.0
       :ocolor (rand-nth ["#d366ff"
                         "#54ffeb"
                          "#fa9eff"])
       :velocity (+ 0.2 (* 0.2 (rand)))}
      (rand-nth form-lib))))

(defn chord-pressed
  "Returns the time the chord was completed"
  [chord ks]
  (when (= (set (keys ks)) chord)
    (apply max (vals ks))))

(defn toggle-debug
  [gs]
  (assoc gs :debug? (not (:debug? gs))))

(def game-state
  (atom init-state))

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
  (let [{:keys [elapsed-time
                size]} game-state
        {:keys [cw ch]} size

        pulse? (< (mod elapsed-time 1.2) 0.6)
        w (if pulse? 5 2)]
    (assoc game-state :caret {:x (- (* cw 0.25) (/ w 2))
                              :y 0
                              :w w
                              :h ch})))


(defn intersects?
  [bit caret]
  (let [{cx :x cw :w} caret
        {bx :x bw :w} bit]
    (and (<= bx cx)
         (<= (+ cx cw) (+ bx bw)))))

(defn maybe-flip-sign
  [n]
  (if (< 0.5 (rand))
    (- n)
    n))

(defn explode
  [form]
  (let [chars (:text (:success form ""))
        value (:value form)
        x (:x form)
        y (:y form)
        xoff (volatile! x)
        {:keys [elapsed-time]} @game-state]

    (set! *score* (+ *score* (or value 0)))

    (oset! cx "font"  "80px Menlo")

    (assoc form :exploding? true
                :bits (-> (mapv
                            (fn [char]
                              (let [text (str char)]
                                {:text text
                                 :x (vswap! xoff + (oget (ocall cx "measureText" text) "width"))
                                 :y y
                                 :wait-until (+ elapsed-time 0.3)
                                 :velocity [(maybe-flip-sign (+ 0.5 (rand 0.5)))
                                            (maybe-flip-sign (+ 0.5 (rand 0.5)))]
                                 :color (rand-nth ["#00FF00"
                                                   "#11FF11"
                                                   "#aaFFaa"])}))
                            chars)
                          (conj {:text (rand-nth ["Perfect!"
                                                  "Nice Job!"
                                                  "Good!"
                                                  "Great!"])
                                 :font "36px Rockwell"
                                 :x (+ x 128 (* (rand) 64))
                                 :y (- y 64 (* (rand) 32))
                                 :wait-until (dec elapsed-time)
                                 :velocity [0.0 -0.8]
                                 :color "#FFFFFF"})))))

(defn update-exploding
  [form]
  (let [{:keys [delta
                elapsed-time]} @game-state
        {:keys [bits velocity]} form]
    (assoc form
      :velocity (+ velocity (* velocity delta))
      :bits (->> bits
                 (mapv (fn [bit]
                         (let [{:keys [velocity
                                       x
                                       y
                                       wait-until]} bit
                               [xd yd] velocity]
                           (if (< elapsed-time wait-until)
                             bit
                             (assoc bit
                               :velocity [(+ xd (* -1 delta xd))
                                          (+ yd (* -1 delta yd))]
                               :x (+ (* 500.0 xd delta) x)
                               :y (+ (* 500.0 yd delta) y))))))))))

(defn win
  [form]
  (let [{:keys [success
                step]} form]
    (cond
      step (merge (dissoc form :step :active-from) step)
      success (explode form)
      :else form)))

(defn update-form
  [form]
  (let [{:keys [bits
                progress
                slot
                chord]} form
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
                         :color (:ocolor form))]
    (if (:exploding? form)
      (update-exploding form)
      (do
        (oset! cx "font" "80px Menlo")
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
                  win? (and active? (> (chord-pressed chord keys) active-from))
                  form (if active?
                         (assoc form
                           :color "#f1f442"
                           :active-from active-from)
                         form)
                  bit (assoc bit :active? active?)]
              (if win?
                (win form)
                (recur (inc i)
                       (+ xacc w)
                       (assoc form
                         :active? (or (:active? form) active?))
                       (assoc bits i bit))))))))))

(defn key-up
  [game-state key-code]
  (update game-state :keys dissoc key-code))

(defn key-down
  [game-state key-code]
  (assoc-in game-state [:keys key-code] (:elapsed-time game-state)))

(defn draw-bit [bit]
  (let [{:keys [text x y font color]} bit]
    (when (some? color)
      (oset! cx "fillStyle" color))
    (when (some? font)
      (oset! cx "font" font))
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
  (let [{:keys [last-time
                score
                forms
                size
                debug?]} game-state
        {:keys [cw ch]} size]
    (draw-caret game-state)
    (run! draw-form forms)

    (oset! cx "fillStyle" "#FFFFFF")
    (oset! cx "font" "80px Rockwell")
    (ocall cx "fillText" (str score) (- cw 128) 64)

    (when debug?
      (puts (with-out-str (cljs.pprint/pprint game-state))))))

(defn filter-forms [forms]
  (filterv (fn [form] (<= (:progress form) 2.0)) forms))

(defn update-forms [game-state]
  (let [{:keys [delta]} game-state]
    (update game-state :forms
            (comp filter-forms
                  (fn [forms]
                    (mapv (fn [f]
                            (let [{:keys [progress velocity]} f
                                  new-progress (+ progress (* delta velocity))]
                              (-> (assoc f :progress new-progress)
                                  (update-form)))) forms))
                  (fn [forms]
                    (if-some [form (generate-form game-state)]
                      (conj forms form)
                      forms))))))

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
    (binding [*score* (:score game-state)]
      (-> (assoc
            game-state
            :size size
            :elapsed-time (+ elapsed-time delta)
            :delta delta)
          (update-caret)
          (update-forms)
          ((fn [g] (assoc g :score *score*)))))))

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
    (swap! game-state key-up (keyboard/key key-code))))

(defn key-down-handler
  [event]
  (let [key-code (oget event "keyCode")]
    (swap! game-state key-down (keyboard/key key-code))))

(defn key-press-handler [event]
  (let [key-code (oget event "keyCode")
        key (keyboard/key key-code)]
    (swap!
      game-state
      (case key
        :num-1 toggle-debug
        identity))))

(defn init-input
  []
  (oset! js/document "onkeydown" (fn [event] (key-down-handler event)))
  (oset! js/document "onkeyup" (fn [event] (key-up-handler event)))
  (oset! js/document "onkeypress" (fn [event] (key-press-handler event))))

(defonce _ (do
             (init-input)
             (game-loop)))


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
