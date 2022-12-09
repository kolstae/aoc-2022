(ns day09
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n\n")

(defn move->points [[x y] [dir n]]
  (case dir
    "D" (map vector (repeat x) (map dec (range y (- y n) -1)))
    "U" (map vector (repeat x) (map inc (range y (+ y n))))
    "L" (map vector (map dec (range x (- x n) -1)) (repeat y))
    "R" (map vector (map inc (range x (+ x n))) (repeat y))))

(defn input->points [input]
  (->> (str/split-lines input)
       (map #(-> % (str/split #"\s") (update 1 parse-long)))
       (reduce (fn [points move]
                 (->> move
                      (move->points (get points (dec (count points))))
                      (into points)))
               [[0 0]])))

(defn tail-positions [points]
  (->> points
       (reductions (fn [t h]
                     (let [[dx dy] (map - h t)]
                       (cond
                         (> dx 1) (cond-> (update t 0 inc)
                                    (not= 0 dy) (update 1 (if (pos? dy) inc dec)))
                         (> dy 1) (cond-> (update t 1 inc)
                                    (not= 0 dx) (update 0 (if (pos? dx) inc dec)))
                         (> -1 dx) (cond-> (update t 0 dec)
                                     (not= 0 dy) (update 1 (if (pos? dy) inc dec)))
                         (> -1 dy) (cond-> (update t 1 dec)
                                     (not= 0 dx) (update 0 (if (pos? dx) inc dec)))
                         :else t)))
                   [0 0])
       next))

(comment

 (let [_input small-input]
   (->> input
        input->points
        tail-positions
        set
        count))
 ;; part-1 6037

 (let [_input small-input
       _input "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"]
   (->> input
        input->points
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        tail-positions
        set
        count))
 ;; part-2 2485
 )
