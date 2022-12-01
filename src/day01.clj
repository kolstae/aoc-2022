(ns day01
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(comment

 (let [_input small-input]
   (->> (str/split input #"\n\n")
        (map (comp (partial apply +)
                   (partial map parse-long)
                   str/split-lines))
        (apply max)))
 ;; part-1 72240

 (let [_input small-input]
   (->> (str/split input #"\n\n")
        (map (comp (partial apply +)
                   (partial map parse-long)
                   str/split-lines))
        (sort >)
        (take 3)
        (apply +)))
 ;; part-2 210957
 )
