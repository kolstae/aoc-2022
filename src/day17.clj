(ns day17
  (:require
   [clojure.string :as str]))

(def input (str/trim (slurp (str "resources/" *ns* ".txt"))))
(def small-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def rocks [[[2 1] [3 1] [4 1] [5 1]]
            [[3 3] [2 2] [3 2] [4 2] [3 1]]
            [[4 3] [4 2] [2 1] [3 1] [4 1]]
            [[2 4] [2 3] [2 2] [2 1]]
            [[2 2] [3 2] [2 1] [3 1]]])

(defn jets->dx [jets rock n]
  (let [min-dx (- (apply min (map first rock)))
        max-dx (- 6 (apply max (map first rock)))]
    (reduce (fn [dx d] (max min-dx (min max-dx (+ dx d))))
            0
            (take n jets))))

(defn move-rock [state jets rock h]
  (let [dx (jets->dx jets rock 4)]
    (loop [jets (drop 4 jets)
           rock (mapv (fn [[x y]] [(+ dx x) (+ h y)]) rock)
           jet-cnt 4]
      (let [one-down (mapv (fn [[x y]] [x (dec y)]) rock)]
        (if (some state one-down)
          [jets rock jet-cnt]
          (let [dx (jets->dx jets one-down 1)
                blown (mapv (fn [[x y]] [(+ dx x) y]) one-down)]
            (recur
             (next jets)
             (if (some state blown) one-down blown)
             (inc jet-cnt))))))))

(defn print-state [state]
  (doseq [s (->> state
                 (group-by second)
                 sort
                 reverse
                 #_(take 10)
                 butlast
                 (map (comp set (partial map first) val)))]
    (println (str/join (map #(if (s %) \# \.) (range 7)))))
  (println "-------"))

(comment

 (let [_input small-input
       state (into #{} (map #(vector % 0)) (range 7))
       jets (cycle (mapv {\< -1 \> 1} input))]
   (loop [state state jets jets [rock & rocks] (cycle rocks) h 0 n 1]
     (let [[jets rock] (move-rock state jets rock h)
           new-h (max (apply max (map second rock)) h)
           state (into state rock)]
       (if (= n 2022)
         (do #_(print-state state)
           new-h)
         (recur state jets rocks new-h (inc n))))))
 ;; part-1 3100

 (let [_input small-input
       state (into #{} (map #(vector % 0)) (range 7))
       jets (cycle (mapv {\< -1 \> 1} input))
       total-jets (count input)]
   (loop [state state jets jets
          [rock & rocks] (cycle rocks)
          h 0 n 1 jet-count 0
          cache {}]
     (let [[jets rock j-cnt] (move-rock state jets rock h)
           jet-count (mod (+ jet-count j-cnt) total-jets)
           ys (distinct (map second rock))
           new-h (apply max h ys)
           state (into state rock)
           cache-key [(mod n 5) jet-count (for [y ys x (range 7) :when (state [x y])] x)]]
       #_(print-state state)
       (if-some [[cycle-start-h cycle-start] (get cache cache-key)]
         (let [rest-n (- 1000000000000 cycle-start)
               cycle-len (- n cycle-start)
               cycle-h (- new-h cycle-start-h)
               extra-n (+ cycle-start (mod rest-n cycle-len))]
           (+ (* (quot rest-n cycle-len) cycle-h)
              (-> (filter (comp #{extra-n} second val) cache)
                  (first)
                  (val)
                  (first))))
         (recur state
                jets
                rocks
                new-h
                (inc n)
                jet-count
                (assoc cache cache-key [new-h n]))))))
 ;; part-2 1540634005751
 )
