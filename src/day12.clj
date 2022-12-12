(ns day12
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")

(defn shortest-path [ns start end]
  (loop [v #{} q {start 0}]
    (if (empty? q)
      nil
      (let [[[y x :as s] d] (apply min-key val q)]
        (if (= s end)
          d
          (let [v (conj v s)
                max-c (inc (int (get-in ns s)))
                d (inc d)]
            (recur v (reduce (fn [m p]
                               (if-some [dc (get-in ns p)]
                                 (if (<= (int dc) max-c)
                                   (update m p (fnil min Long/MAX_VALUE) d)
                                   m)
                                 m))
                             (dissoc q s)
                             (for [p [[(inc y) x] [(dec y) x] [y (dec x)] [y (inc x)]]
                                   :when (not (v p))]
                               p)))))))))

(comment

 (let [_input small-input
       m (->> (str/split-lines input)
              (mapv (partial into [])))
       [end start] (->> (for [[y xs] (map-indexed vector m)
                              [x c] (map-indexed vector xs)
                              :when (#{\E \S} c)]
                          [c [y x]])
                        sort
                        (map second))
       m (-> m
             (assoc-in end \z)
             (assoc-in start \a))]
   (shortest-path m start end))
 ;; part-1 437

 (let [_input small-input
       m (->> (str/split-lines input)
              (mapv (partial into [])))
       [end start] (->> (for [[y xs] (map-indexed vector m)
                              [x c] (map-indexed vector xs)
                              :when (#{\E \S} c)]
                          [c [y x]])
                        sort
                        (map second))
       m (-> m
             (assoc-in end \z)
             (assoc-in start \a))]
   (->> (for [[y xs] (map-indexed vector m)
              [x c] (map-indexed vector xs)
              :when (= \a c)]
          [y x])
        (keep #(shortest-path m % end))
        (apply min)))
 ;; part-2 430
 )
