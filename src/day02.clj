(ns day02
  (:require [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "A Y\nB X\nC Z")

(def decode {"A" :rock
             "B" :paper
             "C" :scissors
             "X" :rock
             "Y" :paper
             "Z" :scissors})

(def hand-score {:rock 1
                 :paper 2
                 :scissors 3})

(def decode-2 (merge decode {"X" :loose
                             "Y" :draw
                             "Z" :win}))

(defn hand->order [hand]
  (->> [:rock :paper :scissors] cycle (drop-while (partial not= hand)) next (take 2)))

(defn score [[h1 h2]]
  (+ (cond
       (= h1 h2) 3
       (->> (hand->order h1) first (= h2)) 6
       :else 0)
     (hand-score h2)))

(comment

 (let [_input small-input]
   (->> (str/split-lines input)
        (map (comp (partial mapv decode) #(str/split % #" ")))
        (map score)
        (apply +)))
 ;; part-1 9177

 (let [_input small-input
       choose-hand (fn [[hand outcome]]
                     [hand (case outcome
                             :loose (last (hand->order hand))
                             :win (first (hand->order hand))
                             hand)])]
   (->> (str/split-lines input)
        (map (comp (partial mapv decode-2) #(str/split % #" ")))
        (map choose-hand)
        (map score)
        (apply +)))
 ;; part-2 12111
 )
