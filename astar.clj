(ns astar
  (require (dijkstra)))


(defn astar (defn dijkstra [src nbrs cost heu dst]
  (filter #(= dst %) (dijkstra-seq src nbrs 
				   (fn [i j] (+ (cost i j)
						(heu j dst))))))


