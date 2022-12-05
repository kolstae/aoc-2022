(ns day05
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")

(defn parse-stacks [s]
  (->> (str/split-lines s)
       (map (comp (partial take-nth 4)
                  (partial drop 1)))
       (apply map (comp (juxt last butlast)
                        (partial drop-while #{\space}) list))
       (into {})))

(defn parse-move [l]
  (let [[cnt [from] [to]] (re-seq #"\d+" l)]
    [(parse-long cnt) from to]))

(comment

 (let [_input small-input
       [state ops] (str/split input #"\n\n")]
   (->> (map parse-move (str/split-lines ops))
        (reduce (fn [state [cnt from to]]
                  (-> state
                      (update to (partial into) (take cnt (get state from)))
                      (update from (partial drop cnt))))
                (parse-stacks state))
        vals
        (map first)
        str/join))
 ;; part-1 SHMSDGZVC

 (let [_input small-input
       [state ops] (str/split input #"\n\n")]
   (->> (map parse-move (str/split-lines ops))
        (reduce (fn [state [cnt from to]]
                  (-> state
                      (update to (partial into) (reverse (take cnt (get state from))))
                      (update from (partial drop cnt))))
                (parse-stacks state))
        vals
        (map first)
        str/join))
 ;; part-2 VRZGHDFBQ
 )
