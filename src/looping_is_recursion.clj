(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         [first & rest :as a-seq] a-seq]
    (when (seq a-seq)
      (if (pred first)
        i
        (recur (inc i) rest)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         a-seq a-seq]
    (if (empty? a-seq)
      (if (zero? count)
        0
        (/ sum count))
      (recur (+ sum (first a-seq))
             (inc count)
             (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      set
      (recur (toggle set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (if (<= n 1)
    n
    (loop [fn-1 0
           fn 1
           n (dec n)]
      (if (zero? n)
        fn
        (recur fn (+ fn-1 fn) (dec n))))))

(defn cut-at-repetition [a-seq]
  (loop [already-seen #{}
         result []
         [first & rest :as a-seq] a-seq]
    (cond (empty? a-seq) result
          (contains? already-seen first)
            (recur already-seen result rest)
          :else (recur (conj already-seen first)
                       (conj result first)
                       rest))))

