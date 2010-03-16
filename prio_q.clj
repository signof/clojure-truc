(ns prio-q)

;
; simple implementation of priority-queues based on sorted-maps
; possibly quite slow
;

(defn make-q [comp] 
  (sorted-map-by comp))

(defn insert [q prio x]
  (merge-with concat q {prio [x]}))

(defn take-first [q]
  (first (second (first q))))

(defn drop-first [q]
  (assoc q (first (first q))
	 (rest (first q))))

(defn rekey [q x old new]
  "moves [x] from [old] to [new] key in [q]"
  (let [one-less (assoc q old (remove #(= x %) (q old)))]
	(insert one-less new x)))

