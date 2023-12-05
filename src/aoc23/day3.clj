(ns aoc23.day3
  (:require [clojure.string :as string]))

(def input
  (-> "resources/day3"
      slurp
      string/split-lines))

(defn empty-row [input]
  (->> (repeat ".")
       (take (-> input first count))
       (apply str)))

(defn pad-and-partition [input]
  (let [empty-row (empty-row input)]
    (->> (concat [empty-row] input [empty-row])
         (partition 3 1))))

(def deltas
  [[-1 -1] [-1 0] [-1 1]
   [0  -1]        [0  1]
   [1  -1] [1  0] [1  1]])

(defn neighbors-at [yx]
  (mapv #(map + yx %) deltas))

(defn modify-element [central-pos neighbor-pos element]
  (let [[_ cx] central-pos
        [_ nx] neighbor-pos
        x-delta (- nx cx)]
    (cond
      (< x-delta 0) (-> element last str)
      (> x-delta 0) (-> element first str)
      :else element)))

(defn neighbors [pos matrix]
  (map #(modify-element pos % (get-in matrix %))
       (neighbors-at pos)))

(defn split-by-vec [split-vec str]
  (let [lengths (map count split-vec)
        indices (reductions + 0 lengths)]
    (mapv (fn [start end] (subs str start (min end (count str))))
          indices
          (rest indices))))

(defn separate-numbers [row]
  (re-seq #"\d+|[^0-9]+" row))

(defn numerical-str? [s]
  (boolean (re-find #"\d+" s)))

(defn special-str? [s]
  (boolean (re-find #"[^a-zA-Z0-9.]" s)))

(defn part-numbers [matrix]
  (let [separated-row (separate-numbers (second matrix))
        num-pos (filter #(numerical-str? (second %))
                        (map-indexed vector separated-row))
        separated-matrix (mapv #(split-by-vec separated-row %) matrix)]
    (->> num-pos
         (filter (fn [[i _]] (some special-str? (neighbors [1 i] separated-matrix))))
         (map (fn [[_ n]] (parse-long n))))))

(defn solve [input]
  (->> (pad-and-partition input)
       (mapcat part-numbers)
       (apply +)))
