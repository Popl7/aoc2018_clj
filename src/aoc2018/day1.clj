(ns aoc2018.day1)

(def lines
  (->>
   "day1.1.txt"
   (clojure.java.io/resource)
   (slurp)
   (clojure.string/split-lines)))

(def numbers
  (->>
   lines
   (map #(read-string %))))


;; day1.1
(defn part1 []
  (->>
   numbers
   (reduce +)))

(def numbers2
  ;[1 -2 3 1 1 -2]
  [1 -1]
  ;[3 3 4 -2 -4]
  ;[-6 3 8 5 -6]
  ;[7 7 -2 -7 -4]
)

;; day1.2
(defn part2 []
  (println "xxxxxxxxxxxxxxxx start xxxxxxxxxxxxxxxx with " (count numbers) " numbers")
  (let [number-loop (cycle numbers)]
    (loop [vals number-loop, previous 0, history (hash-set 0)]
      (let [next_val (first vals)
            new_val (if next_val
                      (+ previous next_val)
                      previous)
            new_history (conj history new_val)
            duplicate (contains? history new_val)]
        (if (not next_val)
          (println "NOT FOUND!")
          (if duplicate
            (do
              (println "FOUND " new_val)
              new_val)
            (recur (rest vals) new_val new_history)))))))
