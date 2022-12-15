(ns day15
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn input->points [input]
  (->> (str/split-lines input)
       (map (fn [s]
              (->> s
                   (re-seq #"-?\d+")
                   (map parse-long)
                   (partition 2)
                   (mapv vec))))))

(defn ys-at [at-y [[x y :as sp] bp]]
  (let [d (apply + (map (comp abs -) sp bp))
        dx (- d (abs (- y at-y)))]
    (when-not (neg? dx)
      (range (- x dx) (inc (+ x dx))))))

(defn xrs-at [[from to] at-y [[x y :as sp] bp]]
  (let [d (apply + (map (comp abs -) sp bp))
        dx (- d (abs (- y at-y)))]
    (when-not (neg? dx)
      [(max from (- x dx)) (min to (+ x dx))])))

(comment

 (let [at-y 2000000
       #_#_[input at-y] [small-input 20]
       points (input->points input)]
   (->> points
        (mapcat (partial ys-at at-y))
        (remove (into #{} (comp (mapcat identity)
                                (filter (comp #{at-y} second))
                                (map first)) points))
        (distinct)
        (count)
        time))
 ;; part-1 5461729

 (let [max-y 4000000
       #_#_[input max-y] [small-input 20]
       points (input->points input)
       [x y] (->> (range (inc max-y))
                  (reduce (fn [all y]
                            (let [xs (->> points
                                          (keep (partial xrs-at [0 max-y] y))
                                          sort
                                          (reduce (fn [[f1 t1] [f2 t2]]
                                                    (cond
                                                      (>= t1 t2) [f1 t1]
                                                      (<= f2 t1) [f1 t2]
                                                      (< t1 f2) (reduced [t1 y])))))]

                              (cond-> xs
                                (not= all xs) reduced)))
                          [0 max-y])
                  time)]
   (+ (* 4000000 x) y))
 ;; part-2 10621647166538
 )
