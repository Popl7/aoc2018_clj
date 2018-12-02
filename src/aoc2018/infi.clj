(ns aoc2018.infi.infi
  (:require [clojure.zip :as z]))

(def lines
  (->>
   "03M4UMUWV209.txt"
   (clojure.java.io/resource)
   (slurp)
   (clojure.string/split-lines)))

;; ║ ╔ ╗ ╠ ╦ ╚ ╝ ╬ ╩ ═ ╣

(def maze
  ["╔═╗║"
   "╠╗╠║"
   "╬╬╣╬"
   "╚╩╩═"])

(def moves-map
  {"║" [[1,0], [-1,0]],
   "╔" [[1,0], [0,1]],
   "╗" [[1,0], [0,-1]],
   "╠" [[1,0], [-1,0], [0,1]],
   "╦" [[1,0], [0,-1], [0,1]],
   "╚" [[-1,0], [0,1]],
   "╝" [[-1,0], [0,-1]],
   "╬" [[-1,0], [1,0], [0,-1], [0,1]],
   "╩" [[-1,0], [0,-1], [0,1]],
   "═" [[0,-1], [0,1]],
   "╣" [[1,0], [-1,0], [0,-1]]})

(def rows maze)
(def cols (first rows))
(def start [0 0])
(def finish [(count rows) (count cols)])

(println "maze " (count rows) " rows " (count cols)  " cols")
(println "start " start " finish " finish)


(defn mk-children [pos path]
  ;;(println "child" pos "path" path)
  (let [row (get rows (first pos))
        shape (get row (last pos))
        moves (get moves-map (str shape))
        ;;_ (println "moves" moves)
        new-poses (map (fn [m]
                         [(+ (first pos) (first m))
                          (+ (last pos) (last m))])
                       moves)
        ;;_ (println "new poses" new-poses)
        valid-poses (filter (fn [p]
                              (not-any? #(= % p) path))
                            new-poses)
        ;;_ (println "valid-poses" valid-poses)
        ;;_ (println "child" pos path new-moves)
        children (map #(mk-children % (conj path %))
                      valid-poses)
        ]
    (if (or (= pos finish)
            (empty? valid-poses))
      {pos {:path path}}
      {pos {:path path children children}}))
  )

(let [pos start
      _ (println "xxxxxxxxxxxxxxxxxxxxxx")
      path [pos]
      row (get rows (first pos))
      shape (get row (last pos))
      moves (get moves-map (str shape))
      ;;_ (println "moves" moves)
      new-poses (map (fn [m]
                       [(+ (first pos) (first m))
                          (+ (last pos) (last m))])
                     moves)
      ;;_ (println "new poses" new-poses)
      ;;_ (println "root" pos "path" path "moves" moves)
      children (map #(mk-children % (conj path %))
                    new-poses)
      tree (z/seq-zip (map-zipper {pos children}))
      ]
  ;; (doall (map #(println (count (:path %))) children))
  ;; (println " => " tree)
  ;;(println " => eager " (bfs-eager tree))
  ;;(println " => lazy " (bfs-lazy tree))
  )



(defn map-zipper [m]
  (z/zipper
   (fn [x] (or (map? x) (map? (nth x 1))))
   (fn [x] (seq (if (map? x) x (nth x 1))))
   (fn [x children]
     (if (map? x)
       (into {} children)
       (assoc x 1 (into {} children))))
   m))


(defn bfs-eager [tree]
  (loop [ret [], queue (conj clojure.lang.PersistentQueue/EMPTY tree)]
    (if (seq queue)
      (let [[node & children] (peek queue)]
        (recur (conj ret node) (into (pop queue) children)))
      ret)))

(defn bfs-lazy [tree]
  ((fn step [queue]
     (lazy-seq
      (when (seq queue)
        (let [[node & children] (peek queue)]
          (cons node
                (step (into (pop queue) children)))))))
   (conj clojure.lang.PersistentQueue/EMPTY tree)))
