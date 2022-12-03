(ns day03
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")

(defn c->prio [c]
  (let [c (int c)]
    (if (> c (int \Z))
      (inc (- c (int \a)))
      (+ 27 (- c (int \A))))))

(comment

 (let [_input small-input]
   (->> (str/split-lines input)
        (mapcat (fn [s] (apply set/intersection (map set (split-at (/ (count s) 2) s)))))
        (map c->prio)
        (apply +)))
 ;; part-1 8085

 (let [_input small-input]
   (->> (str/split-lines input)
        (map set)
        (partition 3)
        (mapcat (partial apply set/intersection))
        (map c->prio)
        (apply +)))
 ;; part-2 2515
 )
