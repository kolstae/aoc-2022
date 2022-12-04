(ns day04
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")

(defn parse-ranges [l]
  (->> (str/split l #",")
       (mapv (comp (fn [[s e]] (set (range s (inc e))))
                   (partial map parse-long)
                   #(str/split % #"-")))))

(comment

 (let [_input small-input]
   (->> (str/split-lines input)
        (map parse-ranges)
        (filter (fn [[r1 r2]] (or (set/superset? r1 r2)
                                  (set/superset? r2 r1))))
        count))
 ;; part-1 605

 (let [_input small-input]
   (->> (str/split-lines input)
        (map parse-ranges)
        (filter (fn [[r1 r2]] (seq (set/intersection r1 r2))))
        count))
 ;; part-2 914
 )
