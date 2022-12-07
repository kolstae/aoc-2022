(ns day07
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k")

(defn build-tree [input]
  (->> (str/split input #"\n?\$\s")
       next
       (reduce (fn [{:keys [cd] :as state} cmd]
                 (let [[op & more] (str/split-lines cmd)
                       [op arg] (str/split op #"\s")]
                   (if (= op "cd")
                     (update state :cd (if (= ".." arg)
                                         pop
                                         #(conj (or % []) arg)))
                     (assoc-in state [:tree cd] (into {}
                                                      (map (fn [s]
                                                             (let [[x name] (str/split s #"\s")]
                                                               (if (= "dir" x)
                                                                 [name (conj cd name)]
                                                                 [name (parse-long x)]))))
                                                      more)))))
               {})
       :tree))

(defn dir-size [tree xs]
  (apply + (map #(if (number? %) % (dir-size tree (tree %))) (vals xs))))

(comment

 (let [_input small-input
       tree (build-tree input)]
   (->> tree
        vals
        (map (partial dir-size tree))
        (filter (partial >= 100000))
        (apply +)))
 ;; part-1 2031851

 (let [_input small-input
       tree (build-tree input)
       tree (update-vals tree (partial dir-size tree))
       needed-size (- 30000000 (- 70000000 (tree ["/"])))]
   (->> tree
        vals
        (filter (partial <= needed-size))
        (apply min)))
 ;; part-2 2568781
 )
