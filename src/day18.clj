(ns day18
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

(let [ops (vec (concat (repeat 3 inc) (repeat 3 dec)))]
  (defn surrounding-cubes [c]
    (map (partial update c) (cycle (range 3)) ops)))

(defn surface-area [cubes]
  (->> cubes
       (mapcat surrounding-cubes)
       (remove cubes)
       count))

(comment

 (let [_input small-input
       cubes (->> (str/split-lines input)
                  (map (comp (partial mapv parse-long)
                             (partial re-seq #"\d+")))
                  set)]
   (surface-area cubes))
 ;; part-1 4288

 (let [_input small-input
       cubes (->> (str/split-lines input)
                  (map (comp (partial mapv parse-long)
                             (partial re-seq #"\d+")))
                  set)
       [min-edge max-edge] (apply (juxt min max) (mapcat identity cubes))
       find-pocket (fn [cubes at-edge? c]
                     (loop [stack #{c} closed #{}]
                       (cond
                         (some at-edge? (mapcat identity stack)) nil
                         (empty? stack) closed
                         :else (recur (into #{}
                                            (comp (remove closed)
                                                  (mapcat surrounding-cubes)
                                                  (remove cubes))
                                            stack)
                                      (into closed stack)))))
       cubes (->> (for [x (range min-edge (inc max-edge))
                        y (range min-edge (inc max-edge))
                        z (range min-edge (inc max-edge))
                        :when (not (cubes [x y z]))]
                    [x y z])
                  (reduce (fn [pockets c]
                            (or (when-not (pockets c)
                                  (when-some [set (find-pocket cubes #{min-edge max-edge} c)]
                                    (into pockets set)))
                                pockets))
                          #{})
                  (into cubes))]
   (surface-area cubes))
 ;; part-2 2494
 )
