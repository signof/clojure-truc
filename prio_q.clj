(ns prio-q)

;
; simple implementation of priority-queues based on sorted-maps
; possibly quite slow
;

(defn make [comp] 
  (sorted-map-by comp))

(defn insert [q prio x]
  (merge-with #(clojure.set/union %1 %2) q {prio #{x}}))

(defn take-first [q]
  (first (second (first q))))

(defn drop-first [q]
  (let [first-elems (rest (second (first q)))]
    (if (empty? first-elems)
      (dissoc q (first (first q)))
      (assoc q (first (first q))
	     (vec first-elems)))))

(defn rekey [q x old new]
  "moves [x] from [old] to [new] key in [q]"
  (let [one-less (assoc q old (remove #(= x %) (q old)))]
	(insert one-less new x)))

