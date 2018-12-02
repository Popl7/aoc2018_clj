(ns aoc2018.day2
  (:require [clojure.set :refer [map-invert]]))


(def lines
  (->>
   "day2.1.txt"
   (clojure.java.io/resource)
   (slurp)
   (clojure.string/split-lines)))

;; (def lines ["aabcdd" "aabddd"])

(defn map-inverse [m]
  (reduce (fn [m' [k v]] (update m' v conj k)) {} m))

(defn count-id [id-nr amount]
  (let [letters (seq (char-array id-nr))
        freq (map-inverse (frequencies letters))]
    (count (get freq amount))))

(let [twos (map #(count-id % 2) lines)
      threes (map #(count-id % 3) lines)
      total-twos (count (filter #(< 0 %) twos))
      total-threes (count (filter #(< 0 %) threes))
      checksum (* total-twos total-threes)]
  (println "checksum " total-twos " * " total-threes " = " checksum))
