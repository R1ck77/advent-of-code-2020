(ns advent-of-code-2020.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *assert* true)

(defn read-double-letter-direction [[first second & rest]]
  (cond
    (and (= first \s) (= second \w)) :sw
    (and (= first \s) (= second \e)) :se
    (and (= first \n) (= second \w)) :nw
    (and (= first \n) (= second \e)) :ne))

(defn read-single-letter-direction [[first & rest]]
  (cond
    (= first \e) :e 
    (= first \w) :w))

(defn- read-path [line]
  (loop [text line
         acc []]
    (if (empty? text)
      acc
      (if-let [direction (read-single-letter-direction text)]
        (recur (rest text) (conj acc direction))
        (if-let [direction (read-double-letter-direction text)]
          (recur (drop 2 text) (conj acc direction)))))))

(defn- read-problem
  ([] (read-problem (io/resource "day24-input.txt")))
  ([location]
   (let [path-lines (string/split-lines (slurp location))]
     (mapv read-path path-lines))))

(def ^:private displacements {:ne [1 1]
                              :e  [1 0]
                              :se [0 -1]
                              :sw [-1 -1]
                              :w  [-1 0]
                              :nw [0 1]})

(defn- path-to-coordinate [path]
  (vec (reduce #(map + % %2) [0 0] (map displacements path))))

(defn- flip-tile [flipped path]
  (let [coord (path-to-coordinate path)]
    ((if (contains? flipped coord)
       disj
       conj) flipped coord)))

(defn- flip-tiles [path-list]
  (reduce flip-tile #{} path-list))

(defn- part-1
  "This was too easy: ominous…"
  [problem]
  (count (flip-tiles problem)))

(defn- compute-neighbors [coord]
  (map #(map + % coord) (map second displacements)))

(defn- compute-interesting-tiles [coord-list]
  (into #{} (apply concat (map compute-neighbors coord-list))))

(defn- count-neighbor-colors
  "Returns two (black white) counters"
  [tiles coord]
  (let [coord-to-color-f #(if (contains? tiles %) [1 0] [0 1])]
   (reduce #(map + % %2) [0 0]
           (map coord-to-color-f
                (compute-neighbors coord)))))

(defn- evolve-tile
  "Returns the tile if it "
  [tiles-at-n coord]
  (let [color (if (contains? tiles-at-n coord) :black :white)
        [black white] (count-neighbor-colors tiles-at-n coord)]
    (case color
      :black (if (or (zero? black) (> black 2))
               nil
               coord)
      :white (if (= black 2) coord))))

(defn- step [tiles-at-n]
  (let [interesting-tiles (compute-interesting-tiles tiles-at-n)
        black-tiles (filter identity (map (partial evolve-tile tiles-at-n) interesting-tiles))]
    (into #{} black-tiles)))

(defn- steps [tiles n]
  (first (drop n (iterate step tiles))))

(defn- part-2 [problem]
  (count (steps (flip-tiles problem) 100)))

(defn day24
  "Print the solution for day 24"
  []
  (println "*** Results for day24:")
  (let [problem (read-problem)]
    (println "Black tiles at day 1:" (part-1 problem))
    (println "Black tiles at day 100:" (part-2 problem))))
