(ns day06)

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(comment

 (let [_input small-input
       _input "bvwbjplbgvbhsrlpgdmjqwftvncz"]
   (->> (partition 4 1 input)
        (map-indexed (fn [i cs] [i (apply distinct? cs)]))
        (drop-while (comp not second))
        ffirst
        (+ 4)))
 ;; part-1 1625

 (let [_input small-input
       _input "bvwbjplbgvbhsrlpgdmjqwftvncz"]
   (->> (partition 14 1 input)
        (map-indexed (fn [i cs] [i (apply distinct? cs)]))
        (drop-while (comp not second))
        ffirst
        (+ 14)))
 ;; part-2 2250
 )
