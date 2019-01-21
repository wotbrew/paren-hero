(ns paren.paredit)

(def start-markers #{\( \[ \{})
(def end-markers #{\) \] \}})

(def config
  {:forward {:move inc
             :start-markers start-markers
             :end-markers end-markers}
   :backward {:move dec
              :start-markers end-markers
              :end-markers start-markers}})

(defn find-char
  "Takes a vector of chars and moves until char (or member of chars) is found"
  [form opts]
  (let [{:keys [char pos direction chars]
         :or {pos 0
              direction :forward}} opts
        {:keys [move]} (get config direction)
        form (vec form)]
    (loop [i pos]
      (let [c (get form i)]
        (cond
          (or (contains? chars c) (= c char)) i
          rest (recur (move i)))))))

(defn find-exp
  "Returns the start and end of the next exp"
  [form opts]
  (let [form (vec form)
        {:keys [start depth pos direction]
         :or {depth 0
              pos 0
              direction :forward}} opts
        {:keys [move start-markers end-markers]} (get config direction)
        c (get form pos)]
    (cond
      (nil? c) {:start start
                :end (dec pos)}
      (and (= c \space)
           (= depth 0)
           start) {:start start
                   :end (dec pos)}
      (and (contains? end-markers c)
           start
           (= depth 1)) {:start start
                         :end pos}
      (contains? start-markers c) (recur form (assoc opts
                                                     :depth (inc depth)
                                                     :start (or start pos)
                                                     :pos (move pos)))
      (contains? end-markers c) (recur form (assoc opts
                                                   :depth (dec depth)
                                                   :pos (move pos)))
      (= c \space) (recur form (assoc opts
                                      :depth depth
                                      :pos (move pos)))
      :else (recur form (assoc opts
                               :start (or start pos)
                               :depth depth
                               :pos (move pos))))))

(defn move-char
  "Removes the char at old-pos and puts it in at new-pos"
  [form opts]
  (let [{:keys [old-pos new-pos]} opts
        c (get form old-pos)
        form' (str
                (subs form 0 old-pos)
                (subs form (inc old-pos)))]
    (str (subs form' 0 new-pos)
         c
         (subs form' new-pos))))

(defn slurp-right
  "Takes a form with a caret | in it and returns a form slurped right"
  [form]
  (let [caret-pos (find-char form {:char \|})
        next-paren (find-char form {:pos caret-pos :chars end-markers})
        {:keys [start end]} (find-exp form {:pos (inc next-paren)})]
    (move-char form {:old-pos next-paren :new-pos end})))






