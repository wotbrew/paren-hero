(ns ^:figwheel-hooks paren.hero
  (:require
   [goog.dom :as gdom]
   [oops.core :refer [ocall oget oset!]]))

;; define your app data so that it doesn't get over-written on reload
(def init-state
  {:elapsed-time 0
   :delta 0
   :screen-size {:cw 0
                 :ch 0}
   :forms [{:progress 0.2
            :slot 0
            :bits [{:text "("}
                   {:text "+ 1 2"
                    :hot? true}
                   {:text ")"}
                   {:text " 3"}]
            :velocity 0.1}]})

(def game-state
  (atom init-state))

(defn init! []
  (reset! game-state init-state))

(def canvas
  (gdom/getElement "canvas"))

(def cx
  (ocall canvas "getContext" "2d"))

(defn get-size
  []
  (let [bounding-rect (ocall canvas "getBoundingClientRect")]
    {:cw (oget bounding-rect "width")
     :ch (oget bounding-rect "height")}))

(defn update-form
  [form]
  (oset! cx "font"  "80px Menlo")
  (let [{:keys [bits]} form
        {:keys [bits x]} (reduce
                           (fn [acc bit]
                             (let [{:keys [bits x]} acc
                                   {:keys [text]} bit
                                   w (oget (ocall cx "measureText" text) "width")
                                   bit (assoc bit :w w :x x)]
                               {:x (+ w x)
                                :bits (conj bits bit)}))
                           {:bits []
                            :x 0}
                           bits)]
    (assoc form :bits bits)))

(defn draw-bit [bit x y]
  (ocall cx "fillText" (:text bit) (+ x (:x bit)) y))

(defn draw-form
  [form]
  (let [{:keys [progress slot bits]} form
        {:keys [size]} @game-state
        {:keys [cw ch]} size
        x (- cw (* progress cw))
        y (+ 100 (* slot (/ ch 10)))]
    (oset! cx "fillStyle" "#FFFFFF")
    (oset! cx "font"  "80px Menlo")
    (run! (fn [bit] (draw-bit bit x y)) bits)))

(defn draw-caret
  [game-state]
  (let [{:keys [size]} game-state
        {:keys [cw ch]} size]
    (oset! cx "fillStyle" "#FFFFFF")
    (ocall cx "fillRect" (* cw 0.25) 0 2 ch)))

(defn draw [game-state]
  (let [{:keys [last-time forms size]} game-state
        {:keys [cw ch]} size]
    (ocall cx "clearRect" 0 0 cw ch)
    (oset! cx "fillStyle" "#333333")
    (ocall cx "fillRect" 0 0 cw ch)
    (draw-caret game-state)
    (run! draw-form forms)))

(defn update-forms [game-state]
  (let [{:keys [delta]} game-state]
    (update game-state :forms
            (fn [forms]
              (map (fn [f]
                     (let [{:keys [progress velocity]} f
                           new-progress (+ progress (* delta velocity))]
                       (-> (assoc f :progress new-progress)
                           (update-form)))) forms)))))

(defonce last-time (atom nil))

(defn game-loop []
  (js/requestAnimationFrame game-loop)
  (let [current-time (.getTime (js/Date.))
        {:keys [cw ch] :as size} (get-size)
        new-state (swap! game-state (fn [game-state]
                                      (update-forms
                                        (let [{:keys [elapsed-time]} game-state
                                              t (or @last-time current-time)
                                              delta (/ (- current-time t) 1000)]
                                          (assoc
                                            game-state
                                            :size size
                                            :elapsed-time (+ elapsed-time delta)
                                            :delta delta)))))]
    (oset! canvas "width" cw)
    (oset! canvas "height" ch)
    (draw new-state)
    (reset! last-time current-time)))

(defonce _ (game-loop))
;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
