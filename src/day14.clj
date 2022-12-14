(ns day14
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n")

(defn input->points [input]
  (->> (str/split-lines input)
       (into #{}
             (mapcat (fn [s]
                       (->> s
                            (re-seq #"\d+")
                            (map parse-long)
                            (partition 2)
                            (partition 2 1)
                            (mapcat (fn [[[x1 y1] [x2 y2]]]
                                      (if (= x1 x2)
                                        (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
                                        (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1]))))))))))

(comment

 (let [_input small-input
       ps (input->points input)
       max-y (apply max (map second ps))]
   (->> ps
        (iterate (fn [ps]
                   (loop [[x y] [500 0]]
                     (when (not= max-y y)
                       (if-let [p (->> [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
                                       (remove ps)
                                       (first))]
                         (recur p)
                         (conj ps [x y]))))))
        next
        (take-while identity)
        count))
 ;; part-1 683

 (let [_input small-input
       ps (input->points input)
       max-y (inc (apply max (map second ps)))]
   (->> ps
        (iterate (fn [ps]
                   (loop [[x y] [500 0]]
                     (if (= max-y y)
                       (conj ps [x y])
                       (if-let [p (->> [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
                                       (remove ps)
                                       (first))]
                         (recur p)
                         (when (pos? y)
                           (conj ps [x y])))))))
        (take-while identity)
        count))
 ;; part-2 28821
 )
