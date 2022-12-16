(ns day16
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II\n")

(defn parse-valves [input]
  (->> (str/split-lines input)
       (map (fn [s]
              (let [[_ from rate valves] (first (re-seq #"Valve (\w+) .*rate=(\d+); .* valves? (.*)" s))]
                [from [(parse-long rate) (str/split valves #"[,\s]+")]])))
       (into {})))

(defn shortest-path [valves start end]
  (loop [v #{} q {start 0}]
    (if (empty? q)
      nil
      (let [[valve d] (apply min-key val q)]
        (if (= valve end)
          d
          (let [v (conj v valve)
                [_ vs] (get valves valve)
                d (inc d)]
            (recur v (reduce (fn [q v]
                               (update q v (fnil min Long/MAX_VALUE) d))
                             (dissoc q valve)
                             (remove v vs)))))))))

(defn all-shortest-paths [valves]
  (let [wanted-valves (->> (into #{}
                                 (comp (filter (comp pos? first second))
                                       (map first))
                                 valves))]
    (into {}
          (map (juxt identity
                     #(into {}
                            (comp (remove #{%})
                                  (map (juxt identity (partial shortest-path valves %))))
                            wanted-valves)))
          (keys valves))))

(comment

 (let [_input small-input
       most-pressure (fn most-pressure [valves valve-dists visited valve pressure minutes]
                       (if (pos? minutes)
                         (let [visited (conj visited valve)
                               minutes (dec minutes)]
                           (transduce
                            (comp (remove (comp visited key))
                                  (keep (fn [[valve dist]]
                                          (let [[delta-p] (get valves valve)
                                                minutes (- minutes dist)
                                                new-pressure (+ pressure (* delta-p minutes))]
                                            (most-pressure valves valve-dists visited valve new-pressure minutes)))))
                            max
                            pressure
                            (get valve-dists valve)))
                         (when (zero? minutes) pressure)))
       valves (parse-valves input)
       valve-dists (all-shortest-paths valves)]
   (time
    (most-pressure valves valve-dists #{} "AA" 0 30)))
 ;; part-1 1789

 (let [_input small-input
       most-pressure (fn most-pressure [valves valve-dists visited [e e-m] [me me-m] pressure]
                       #_(prn [e e-m] [me me-m] pressure visited)
                       (let [visited (conj visited e)
                             me-m (dec me-m)
                             e-m (dec e-m)]
                         (reduce (fn [max-pressure [me dist]]
                                   (if (pos? (- me-m dist))
                                     (let [me-m (- me-m dist)
                                           [delta-p] (get valves me)
                                           visited (conj visited me)
                                           me-pressure (+ pressure (* delta-p me-m))]
                                       (transduce
                                        (comp (remove (comp visited key))
                                              (keep (fn [[valve dist]]
                                                      (let [minutes (- e-m dist)]
                                                        (when-not (neg? minutes)
                                                          (most-pressure valves valve-dists visited
                                                                         [valve minutes]
                                                                         [me me-m]
                                                                         (-> valves (get valve) (first) (* minutes) (+ me-pressure))))))))
                                        max
                                        max-pressure
                                        (get valve-dists e)))
                                     max-pressure))
                                 pressure
                                 (remove (comp visited key) (get valve-dists me)))))
       valves (parse-valves input)
       valve-dists (all-shortest-paths valves)]
   (time
    (most-pressure valves valve-dists #{}
                   ["AA" 26]
                   ["AA" 26]
                   0)))
 ;; part-2 2496
 )
