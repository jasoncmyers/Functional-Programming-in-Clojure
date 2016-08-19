(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base pow]
                 (cond
                   (== pow 0) 1
                   (== pow 1) acc
                   :else (recur (* acc base) base (dec pow))))]
      (helper base base exp)))

(defn last-element [a-seq]
  (if (nil? (next a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2))  false
    (not= (first seq1) (first seq2))  false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         index 0]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur pred (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [acc 0
         num-terms 0
         a-seq a-seq]
    (if (empty? a-seq)
      (if (== num-terms 0)
        0
        (/ acc num-terms))
      (recur (+ acc (first a-seq)) (inc num-terms) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if(contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odd-set #{}
           a-seq a-seq]
      (if (empty? a-seq)
        odd-set
        (recur (toggle odd-set (first a-seq)) (rest a-seq))))))


(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

