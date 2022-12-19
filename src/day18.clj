(ns day18
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

(let [ops (vec (concat (repeat 3 inc) (repeat 3 dec)))]
  (defn surrounding-cubes [c]
    (map (partial update c) (cycle (range 3)) ops)))

(comment

 (let [_input small-input
       cubes (->> (str/split-lines input)
                  (map (comp (partial mapv parse-long)
                             (partial re-seq #"\d+")))
                  set)]
   (->> cubes
        (mapcat surrounding-cubes)
        (remove cubes)
        count))
 ;; part-1 4288

 (let [_input small-input
       cubes (->> (str/split-lines input)
                  (map (comp (partial mapv parse-long)
                             (partial re-seq #"\d+")))
                  set)
       empty-cubes (into #{}
                         (comp (mapcat surrounding-cubes)
                               (remove cubes))
                         cubes)
       cubes (->> (loop [sets [#{(first empty-cubes)}] [c & cs] (next empty-cubes)]
                    (if c
                      (let [s-cs (surrounding-cubes c)]
                        (if-some [ss (seq (filter #(some % s-cs) sets))]
                          (recur (conj (remove (set ss) sets) (conj (apply set/union ss) c)) cs)
                          (recur (conj sets #{c}) cs)))
                      sets))
                  (remove (fn [cs]
                            (->> cs
                                 (into #{} (comp (mapcat surrounding-cubes)
                                                 (remove cs)
                                                 (remove cubes)))
                                 seq)))
                  (mapcat identity)
                  #_(into cubes))]
   (sort cubes)
   #_(->> cubes
        (mapcat surrounding-cubes)
        (remove cubes)
        count))
 ;; TOO HIGH 3962
 ;; part-2
 )
