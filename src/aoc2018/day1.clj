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
  [1 -2 3 1 1 -2])

;; day1.2
(defn part2 []
  (loop [vals (cycle numbers2) prev [0]]
    (let [val (first vals)
          ;;_ (println "val" val)
          new_val (+ (last prev) (or val 0))
          ;;_ (println "nxt" new_val)
          double (some #(= % new_val) prev)]
      ;;(println "here " vals prev double new_val)
      (if (or (not val) double)
        new_val
        (recur (rest vals) (conj prev new_val))))))
