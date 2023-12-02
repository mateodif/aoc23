(ns aoc23.day2
  (:require [clojure.string :as string]))

(def input
  (-> "resources/day2"
      slurp
      string/split-lines))

;; Part 1

(def limit
  {:red 12
   :green 13
   :blue 14})

(def game-regex
  #"Game \d+: ")

(defn get-id [s]
  (->> s
       (re-find game-regex)
       (re-find #"\d+")
       parse-long))

(defn get-clean-sets [s]
  (string/replace s game-regex ""))

(defn get-sets [s]
  (string/split s #"; "))

(defn get-subsets [s]
  (string/split s #", "))

(defn parse-subset [s]
  (let [cube-color #(keyword (re-find #"blue|red|green" %))
        cube-num #(parse-long (re-find #"\d+" %))]
    ((juxt cube-color cube-num) s)))

(defn parse-subsets [subset]
  (map parse-subset subset))

(defn parse-game [game]
  (->> game
       get-clean-sets
       get-sets
       (map get-subsets)
       (map parse-subsets)))

(defn possible-subset? [subset]
  (every? (fn [[k v]] (>= (k limit) v)) subset))

(defn possible-game? [game]
  (->> game
       parse-game
       (every? possible-subset?)))

(defn solve [game]
  (if (possible-game? game)
    (get-id game)
    0))

(def part1
  (apply + (map solve input)))

;; Part 2

(def colors #{:blue :red :green})

(defn max-by-key [k coll]
  {k (k (apply max-key #(k % 0) coll))})

(defn max-by-keys [ks coll]
  (into {} (map #(max-by-key % coll) ks)))

(defn minimum-possible-set [game]
  (->> game
       parse-game
       (map (partial into {}))
       (max-by-keys colors)))

(defn power-of-set [set]
  (apply * (vals set)))

(defn solve2 [game]
  (-> game
      minimum-possible-set
      power-of-set))

(def part2
  (apply + (map solve2 input)))
