(ns aoc23.day1
  (:require [clojure.string :as string]))

;; Part 1

(def input
  (-> "resources/day1"
      slurp
      string/split-lines))

(defn first-and-last [coll]
  (str (first coll) (last coll)))

(defn solve [s]
  (->> s
       (re-find #"\d")
       first-and-last
       parse-long))

(def part1
  (apply + (map solve input)))

;; Part 2

(def spelled-digits
  {"one"   1
   "two"   2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9})

(def digit?
  (re-pattern (str "(?=(\\d|" (string/join "|" (keys spelled-digits)) "))")))

(defn solve2 [s]
  (->> s
       (re-seq digit?)
       (map second)
       (map #(or (spelled-digits %) %))
       first-and-last
       parse-long))

(def part2
  (apply + (map solve2 input)))
