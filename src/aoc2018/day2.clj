(ns aoc2018.day2
  (:require [clojure.set :refer [map-invert difference]]))

(def lines
  (->>
   "day2.1.txt"
   (clojure.java.io/resource)
   (slurp)
   (clojure.string/split-lines)))

;;(def lines ["aabcdd" "aabddd"])
;; (def lines ["abcde"
;;              "fghij"
;;              "klmno"
;;              "pqrst"
;;              "fguij"
;;              "axcye"
;;             "wvxyz"
;;             "fgaaa"])

(defn map-inverse [m]
  (reduce (fn [m' [k v]] (update m' v conj k)) {} m))

(defn count-id [id-nr amount]
  (let [letters (seq (char-array id-nr))
        freq (map-inverse (frequencies letters))]
    (count (get freq amount))))

(defn part1 []
  (let [twos (map #(count-id % 2) lines)
        threes (map #(count-id % 3) lines)
        total-twos (count (filter #(< 0 %) twos))
        total-threes (count (filter #(< 0 %) threes))
        checksum (* total-twos total-threes)]
    (println "checksum " total-twos " * " total-threes " = " checksum)))



(defn stevenshtein [str1 str2]
  (let [seq1  (seq (char-array str1))
        seq2  (seq (char-array str2))
        same (map-indexed #(if (= %2 (nth seq1 %1))
                             %2) seq2)
        same (remove nil? same)]
    (if (= (count same) (dec (count str1)))
      same)))

(defn part2 []
  (let [similars (map (fn [l]
                        (let [similar (map #(stevenshtein l %) lines)
                              similar (remove nil? similar)]
                          [l similar]))
                      lines)
        matched (filter (fn [[k v]] (seq v)) similars)
        matched (map (fn [[k v]] v) matched)]
    (println "matched" (apply str (first (first matched))))))
