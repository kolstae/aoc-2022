(ns day08
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "30373\n25512\n65332\n33549\n35390")

(defn take-until [pred coll]
  (loop [more coll res []]
    (if (seq more)
      (let [v (first more)]
        (if (pred v)
          (conj res v)
          (recur (next more) (conj res v))))
      res)))

(comment

 (let [_input small-input
       m (->> (str/split-lines input)
              (mapv (partial mapv #(- (int %) (int \0)))))
       visible? (fn [y x] (let [w (count (first m))
                                h (count m)
                                lower? (partial > (get-in m [y x]))]
                            (or (every? (comp lower? #(get-in m [y %])) (range x))
                                (every? (comp lower? #(get-in m [% x])) (range y))
                                (every? (comp lower? #(get-in m [y %])) (range (inc x) w))
                                (every? (comp lower? #(get-in m [% x])) (range (inc y) h)))))]
   (->> (for [y (range (count m))
              x (range (count (first m)))]
          (visible? y x))
        (filter boolean)
        (count)))
 ;; part-1 1859

 (let [_input small-input
       m (->> (str/split-lines input)
              (mapv (partial mapv #(- (int %) (int \0)))))
       view-dist (fn [y x] (let [value (get-in m [y x])
                                 eq? #{value}
                                 ge? (partial >= value)]
                             (* (->> (range (dec x) -1 -1) (map #(get-in m [y %])) (take-while ge?) (take-until eq?) (count))
                                (->> (range (dec y) -1 -1) (map #(get-in m [% x])) (take-while ge?) (take-until eq?) (count))
                                (->> (range (inc x) (count (first m))) (map #(get-in m [y %])) (take-while ge?) (take-until eq?) (count))
                                (->> (range (inc y) (count m)) (map #(get-in m [% x])) (take-while ge?) (take-until eq?) (count)))))]
   (->> (for [y (range 1 (dec (count m)))
              x (range 1 (dec (count (first m))))]
          (view-dist y x))
        (apply max)))
 ;; part-2 332640
 )
