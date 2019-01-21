(ns paren.paredit)

(def start-markers #{\( \[ \{})
(def end-markers #{\) \] \}})
(def caret-marker \|)

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
          (nil? c) nil
          :else (recur (move i)))))))

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

(defn remove-chars
  "Replaces the char at given positions"
  [form & positions]
  (let [{:keys [pos s]} (reduce
                          (fn [acc position]
                            (let [{:keys [pos s]} acc]
                              {:s (str s (subs form pos position))
                               :pos (inc position)}))
                          {:pos 0
                           :s ""}
                          positions)]
    (str s (subs form pos))))

(defmulti paredit-op (fn [form op] op))

(defmethod paredit-op :slurp-right
  [form]
  (let [caret-pos (find-char form {:char caret-marker})
        _ (assert (some? caret-pos) "Couldn't find caret")
        next-paren (find-char form {:pos caret-pos :chars end-markers})
        {:keys [start end]} (find-exp form {:pos (inc next-paren)})]
    (move-char form {:old-pos next-paren :new-pos end})))

(defmethod paredit-op :splice
  [form _]
  (let [caret-pos (find-char form {:char caret-marker})
        _ (assert (some? caret-pos "Couldn't find caret"))
        end (find-char form {:chars end-markers
                             :pos caret-pos
                             :direction :forward})
        start (find-char form {:chars start-markers
                               :pos caret-pos
                               :direction :backward})]
    (remove-chars form start end)))



