(ns astar)

(defn astar [src nbrs cost heu dst]
  (filter #(= dst %) (dijkstra/dijkstra-seq src nbrs 
					    (fn [i j] (+ (cost i j)
							 (heu j dst))))))

(defmacro astar-path [& args]
  "Like astar but fetches path"
  `(:path (astar ~@args)))
(defmacro astar-cost [& args]
  "Like astar but fetches cost"
  `(:cost (astar ~@args)))


