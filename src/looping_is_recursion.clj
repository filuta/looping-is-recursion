(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))


(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))


(defn find-first-index [pred a-seq]
  (loop [l-pred pred
         l-seq a-seq
         index 0]
    (cond
     (empty? l-seq) nil
     (l-pred (first l-seq)) index
     :else (recur l-pred (rest l-seq) (inc index)))))


(defn avg [a-seq]
  (loop [sum (first a-seq)
         numb 1
         l-seq (rest a-seq)]
    (if (empty? l-seq)
      (/ sum numb)
      (recur (+ sum (first l-seq))
             (inc numb)
             (rest l-seq)))))


(defn parity [a-seq]
  (loop [l-seq a-seq
         parity-set #{}]
    (let [toggle (fn [a-set elem]
                   (if (contains? a-set elem)
                     (disj a-set elem)
                     (conj a-set elem)))]
      (if (empty? l-seq)
        parity-set
        (recur (rest l-seq) (toggle parity-set (first l-seq)))))))


(defn fast-fibo [n]
  (loop [i 2
         x 0
         y 1]
    (cond
     (== n 0) 0
     (== n 1) 1
     (== n i) (+ x y)
     :else (recur (inc i) y (+ x y)))))


(defn cut-at-repetition [a-seq]
  (loop [remaining a-seq
         result (empty a-seq)
         elems #{}]
    (if
      (or (empty? remaining)
          (contains? elems (first remaining)))
      result
      (recur
       (rest remaining)
       (conj result (first remaining))
       (conj elems (first remaining))))))


