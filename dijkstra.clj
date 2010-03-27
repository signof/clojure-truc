(ns dijkstra)

(defstruct q-node :node :cost :path)
(defn- q-node-for-source [v]
  (struct q-node v 0 []))
(defn- q-nodes-for-neighbours [vq nbrs cost]
  "generates new nodes for insertion into PQ for all the neighbours 
  of the unerlying graph node of the PQ node [vq]. Neighbours are 
  determined by the [nbrs] function, cost of edges by [cost]"
  (let [{v :node vc :cost vp :path} vq]    
    (for [n (nbrs v)]
      (struct q-node 
              n 
              (+ vc (cost v n)) 
              (conj (:path vq) n)))))
(defn- new-pq 
  ([] (prio-q/make compare)) 
  ([src] (prio-q/insert (new-pq) 0 (q-node-for-source src))))
(defn- add-nodes [pq nodes] 
  (reduce (fn [pq n] (prio-q/insert pq (:cost n) n)) pq nodes))

;
;
(defn dijkstra-seq [src nbrs cost]
  "Lazily calculates the shortest path (according to [cost]) from 
  the source [src] to all processes. The graph is defined by the 
  [nbrs] function, which should return a collection of neighbours 
  when supplied a node."
  (letfn [(do-dij [pq set]
		  (when-let [n (prio-q/take-first pq)]
		    (let [pq (prio-q/drop-first pq)]
		      (if (contains? set n)
			(recur pq set)
			(lazy-seq 
			 (cons n
			       (do-dij (into pq (q-nodes-for-neighbours n nbrs cost))
				       (conj set n))))))))]
    (do-dij (new-pq) #{})))

(defn dijkstra [src nbrs cost dst]
  (filter #(= dst %) (dijkstra-seq src nbrs cost)))

(defmacro dijkstra-path [& args]
  "Like dijkstra but fetches path"
  `(:path (dijkstra ~@args)))
(defmacro dijkstra-cost [& args]
  "Like dijkstra but fetches cost"
  `(:cost (dijkstra ~@args)))

