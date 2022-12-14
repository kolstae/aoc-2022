(ns day13
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]\n\n")

(defn ->vec [x] (if (vector? x) x (vector x)))

(defn validate-pkg [p1 p2]
  (loop [[a & p1] p1 [b & p2] p2 more nil]
    (cond
      (= a b) (if (or p1 p2)
                (recur p1 p2 more)
                (when-some [[[p1 p2]] (seq more)]
                  (recur p1 p2 (next more))))

      (some nil? [a b])
      (and (nil? a) (not (nil? b)))

      (some vector? [a b])
      (recur (->vec a) (->vec b) (cond-> more
                                   (or p1 p2) (conj [p1 p2])))

      :else (< a b))))

(comment

 (validate-pkg [[[[6] [9 8 5]]]]
               [[6]])
 (validate-pkg [[8,[],[10]],[]]
               [[[[8,5,6,6,5],1,[10]],[]],[],[],[7],[2,2]]
               )

 (let [_input small-input]
   (->> (str/split-lines input)
        (filter seq)
        (map edn/read-string)
        (partition 2)
        (map (partial apply validate-pkg))
        (map-indexed vector)
        (filter second)
        (map (comp inc first))
        (apply +)))
 ;; part-1 6235

 (let [_input small-input]
   (->> (str/split-lines input)
        (filter seq)
        (map edn/read-string)
        (concat [[[2]] [[6]]])
        (sort (comparator validate-pkg))
        (map-indexed vector)
                (filter (comp #{[[2]] [[6]]} second))
                (map (comp inc first))
                (apply *)))
 ;; part-2 22866
 )
