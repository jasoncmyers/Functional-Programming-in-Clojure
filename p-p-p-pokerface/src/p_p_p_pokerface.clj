(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        repl {\T 10, \J 11, \Q 12, \K 13 \A 14}]
    (cond
      (Character/isDigit rnk) (Integer/valueOf (str rnk))
      :else (repl rnk))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        f     (vals (frequencies ranks))]
    (>= (apply max f) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        f     (vals (frequencies ranks))]
    (>= (apply max f) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        f     (vals (frequencies ranks))]
    (>= (apply max f) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        f     (vals (frequencies suits))]
    (== 1 (count f))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        f (vals (frequencies ranks))]
    (= (sort f) [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        f (vals (frequencies ranks))]
    (or (= (sort f) [1 2 2]) (= (sort f) [1 4]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sort-hand (sort ranks)
        sort-ace-low (sort (replace {14 1} ranks))
        min-val (apply min ranks)
        ace-low-min (apply min sort-ace-low)]
    (or
          (= sort-hand (range min-val (+ min-val 5)))
          (= sort-ace-low (range ace-low-min (+ ace-low-min 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        results (filter #((first %) hand) checkers)]
    (apply max (map second results))))
