(ns paren.keyboard
  (:require [clojure.set :as set])
  (:refer-clojure :exclude [key]))

(def key-code
  {:ctrl-left 17
   :a 65
   :d 68
   :s 83
   :caps-lock 20
   :num-1 49
   })

(def key
  (set/map-invert key-code))