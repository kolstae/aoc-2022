(ns day11
  (:require
   [clojure.string :as str]))

(def input (slurp (str "resources/" *ns* ".txt")))
(def small-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")

(defn parse-monkey [s]
  (let [[id start-items op test & targets] (str/split-lines s)]
    {:monkey (->> id (re-seq #"\d+") first parse-long)
     :items (->> start-items (re-seq #"\d+") (mapv parse-long))
     :op (let [val (some->> op (re-seq #"\d+") first parse-long)
               sym (->> op (re-seq #"[*+]") first symbol resolve)]
           (if (nil? val) #(sym % %) #(sym val %)))
     :test (->> test (re-seq #"\d+") first parse-long)
     :targets (mapv #(->> % (re-seq #"\d+") first parse-long) targets)}))

(defn do-round [worry-fn monkeys]
  (reduce (fn [monkeys monkey-id]
            (let [{:keys [items op test targets]} (get monkeys monkey-id)
                  transfer (map (fn [worry]
                                  (let [new-worry (worry-fn (op worry))]
                                    [(if (zero? (mod new-worry test))
                                       (first targets) (second targets))
                                     new-worry]))
                                items)]
              (-> (reduce (fn [monkeys [monkey-id item]]
                            (update-in monkeys [monkey-id :items] conj item))
                          monkeys
                          transfer)
                  (update monkey-id #(-> %
                                         (assoc :items [])
                                         (update :inspected (fnil + 0) (count transfer)))))))
          monkeys
          (range (count monkeys))))

(defn gcd [a b]
  (if (pos? (min a b))
    (recur b (mod a b))
    (max a b)))

(defn lcm [a b]
     (* a (/ b (gcd a b))))

(comment

 (let [_input small-input
       monkeys (->> (str/split input #"\n\n")
                    (mapv parse-monkey))
       worry-fn #(long (/ % 3))]
   (->> (nth (iterate (partial do-round worry-fn) monkeys) 20)
        (map :inspected)
        (sort >)
        (take 2)
        (apply *)))
 ;; part-1 107822

 (let [_input small-input
       monkeys (->> (str/split input #"\n\n")
                    (mapv parse-monkey))
       lcm (reduce lcm (map :test monkeys))]
   (->> (nth (iterate (partial do-round #(mod % lcm)) monkeys) 10000)
        (map :inspected)
        (sort >)
        (take 2)
        (apply *)))
 ;; part-2 27267163742
 )
