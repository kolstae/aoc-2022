(ns day19
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn parse-blueprint [s]
  (let [[id] (re-seq #"\d+" s)]
    [(parse-long id)
     (into {}
           (map (fn [[_ r-type & rs]]
                  [(keyword r-type) (->> rs
                                         (keep identity)
                                         (partition 2)
                                         (map (fn [[n r]] [(keyword r) (parse-long n)]))
                                         (into {}))]))
           (re-seq #"Each (\w+) robot costs (\d+) (\w+)(?: and (\d+) (\w+))?" s))]))

(defn -or-noop [& ns]
  (apply - (map #(or % 0) ns)))

(defn +or-noop [& ns]
  (apply + (map #(or % 0) ns)))

(defn can-build-bp? [resources bp robot]
  (some->> (bp robot)
           (merge-with -or-noop resources)
           vals
           (not-any? neg?)))

(defn can-ever-build-bp? [n resources robots bp]
  (let [more-needed? (merge-with <= resources (update-vals (apply merge-with max (vals bp)) inc))
        resources (->> (update-vals robots (partial * n))
                       (merge-with +or-noop resources))]
    (filter (every-pred more-needed? (partial can-build-bp? resources bp)) [:geode :obsidian :clay :ore])))

(defn build-robot [{:keys [resources robots bp n] :as state} build-next]
  (if (zero? n)
    state
    (let [state (-> state
                    (update :n dec)
                    (update :resources (partial merge-with +or-noop) robots))]
      (if (can-build-bp? resources bp build-next)
        (-> state
            (update-in [:robots build-next] (fnil inc 0))
            (update :build conj build-next)
            (update :resources (partial merge-with -or-noop) (bp build-next)))
        (recur state build-next)))))

(defn find-max-geode [minutes bp]
  (loop [{:keys [n resources robots bp]
          :as state} {:bp bp
                      :n minutes
                      :build []
                      :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
                      :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}}
         q nil
         max-state {:resources {:geode 0}}]
    #_(when (= build [:ore :clay :clay :clay :clay :clay :clay])
        (clojure.pprint/pprint state))
    (if (zero? n)
      (if-some [[[q-state [b & bs]] & q] (seq q)]
        (do #_(clojure.pprint/pprint (select-keys q-state [:n :build]))
          (recur (build-robot q-state b)
                 (cond-> q
                   (seq bs) (conj [q-state bs]))
                 (max-key (comp :geode :resources) max-state state)))
        (max-key (comp :geode :resources) max-state state))
      (if-some [[b & bs] (seq (can-ever-build-bp? n resources robots bp))]
        (recur (build-robot state b)
               (cond-> q
                 (seq bs) (conj [state bs]))
               max-state)
        (if-some [[[q-state [b & bs]] & q] (seq q)]
          (do #_(clojure.pprint/pprint (select-keys q-state [:n :build]))
            (recur (build-robot q-state b)
                   (cond-> q
                     (seq bs) (conj [q-state bs]))
                   max-state))
          max-state)))))

(comment

 (let [input small-input
       bs (->> (str/split-lines input)
               (map parse-blueprint))
       minutes 24]
   (->> bs
        (map (comp #_(partial apply *)
              (juxt first (comp (partial find-max-geode minutes) second))))
        #_(apply +)))
 ;; part-1 1150


 (let [_input small-input
       bs (->> (str/split-lines input)
               (map parse-blueprint))
       minutes 32]
   (->> bs
        (take 3)
        (map (comp :geode :resources (partial find-max-geode minutes) second))))
 (* 11 79 43)
 ;; part-2 37367
 )
