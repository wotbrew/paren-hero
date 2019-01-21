(ns paren.paredit)

(def start-markers #{\( \[ \{})
(def end-markers #{\) \] \}})
(def caret-marker \|)

(def config
  {:forward {:delta 1
             :start-markers start-markers
             :end-markers end-markers}
   :backward {:delta -1
              :start-markers end-markers
              :end-markers start-markers}})

(defn find-char
  "Takes a vector of chars and moves until char (or member of chars) is found"
  [form opts]
  (let [{:keys [char pos direction chars]
         :or {pos 0
              direction :forward}} opts
        {:keys [delta]} (get config direction)
        form (vec form)]
    (loop [i pos]
      (let [c (get form i)]
        (cond
          (or (contains? chars c) (= c char)) i
          (nil? c) nil
          :else (recur (+ i delta)))))))

(defn find-exp
  "Returns the left and right bounds of the next exp"
  [form opts]
  (let [form (vec form)
        {:keys [start depth pos direction]
         :or {depth 0
              pos 0
              direction :forward}} opts
        {:keys [delta start-markers end-markers]} (get config direction)
        c (get form pos)]
    (cond
      (nil? c) {:start start
                :end nil}
      (and (= c \space)
           (= depth 0)
           start) {:start start
                   :end (- pos delta)}
      (and (contains? end-markers c)
           (= depth 0)) {:start nil
                         :end pos}
      (and (contains? end-markers c)
           start
           (= depth 1)) {:start start
                         :end pos}
      (contains? start-markers c) (recur form (assoc opts
                                                     :depth (inc depth)
                                                     :start (or start pos)
                                                     :pos (+ pos delta)))
      (contains? end-markers c) (recur form (assoc opts
                                                   :depth (dec depth)
                                                   :pos (+ pos delta)))
      (= c \space) (recur form (assoc opts
                                      :depth depth
                                      :pos (+ pos delta)))
      :else (recur form (assoc opts
                               :start (or start pos)
                               :depth depth
                               :pos (+ pos delta))))))

(defn move-char
  "Removes the char at from and puts it in at to"
  [form opts]
  (let [{:keys [from to]} opts
        c (get form from to)
        form' (str
                (subs form 0 from)
                (subs form (inc from)))]
    (str (subs form' 0 to)
         c
         (subs form' to))))

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

(defn find-enclosing-exp
  [form opts]
  (let [{:keys [pos]} opts
        end (loop [i (inc pos)]
              (let [{:keys [start end]} (find-exp form {:pos i
                                                        :direction :forward})]
                (if (nil? start)
                  end
                  (recur (inc end)))))
        start (loop [i pos]
                (let [{:keys [start end]} (find-exp form {:pos i
                                                          :direction :backward})]
                  (if (nil? start)
                    end
                    (recur (dec end)))))]
    {:start start
     :end end}))

(defmulti do-paredit-op (fn [opts] (:op opts)))

(defmethod do-paredit-op :slurp-forward
  [opts]
  (let [{:keys [form pos]} opts
        next-paren (find-char form {:pos pos :chars end-markers})
        {:keys [start end]} (find-exp form {:pos (inc next-paren)})]
    (move-char form {:from next-paren :to end})))

(defmethod do-paredit-op :slurp-backward
  [opts]
  (let [{:keys [form pos]} opts
        prev-paren (find-char form {:pos pos :chars start-markers
                                    :direction :backward})
        {:keys [start end]} (find-exp form {:pos (dec prev-paren)
                                            :direction :backward})]
    (move-char form {:from prev-paren :to end})))

(defmethod do-paredit-op :splice
  [opts]
  (let [{:keys [form pos]} opts
        {:keys [start end]} (find-enclosing-exp form {:pos pos})]
    (remove-chars form start end)))

(defmethod do-paredit-op :barf-forward
  [opts]
  (let [{:keys [form pos]} opts
        {from :end} (find-enclosing-exp form {:pos pos})
        {intermediate :end} (find-exp form {:pos (dec from)
                                            :direction :backward})
        {to :start} (find-exp form {:pos (dec intermediate)
                                    :direction :backward})]
    (move-char form {:from from
                     :to (inc to)})))

(defmethod do-paredit-op :barf-backward
  [opts]
  (let [{:keys [form pos]} opts
        {from :start} (find-enclosing-exp form {:pos pos})
        {intermediate :end} (find-exp form {:pos (inc from)
                                            :direction :forward})
        {to :start} (find-exp form {:pos (inc intermediate)
                                    :direction :forward})]
    (move-char form {:from from
                     :to (dec to)})))

(defn paredit-op
  [opts]
  (let [{:keys [form op]} opts
        pos (find-char form {:pos 0
                             :char caret-marker})]
    (do-paredit-op {:form form
                    :pos pos
                    :op op})))

(comment
  (paredit-op {:form "(3 | 1 (+ 4 1)) 3   2)"
               :op :barf-forward})

  (paredit-op {:form "(3 ([+ -] 4| 1))"
               :op :barf-backward})

  (paredit-op {:form "(3 (+ (+ | 2 2) [4 1] 1) 2)"
               :op :slurp-forward}))
