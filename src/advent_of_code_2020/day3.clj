(ns advent-of-code-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def conversion-map {"#" :tree,
                     "." :empty})

(defprotocol Map
  (getDistance [this] "Get the distance to travel in blocks (the rows of the map) ")
  (getPeriod [this row]
    "Get the map width provided for the specified line.

It should be technically the same for each line, but you never know…")
  (getElement [this pos] "Returns :empty or :tree."))

(defn- data-to-symbol [char]
  ({\# :tree, \. :empty} char))

(defn- get-map [lines]
  (reify Map
    (getDistance [this]
      (count lines))
    (getPeriod [this row]
      (count (nth lines row)))
    (getElement [this [row column]]
      (let [line (nth lines row)
            line-width (count line)
            raw-char (nth line (mod column line-width))]
        (data-to-symbol raw-char)))))

(defn- next-move [position move]
  (map + move position))

(defn- create-path [zone-map move]
  (let [max-distance (.getDistance zone-map)]
   (take-while #(< (first %) max-distance)
               (iterate #(next-move % move) '(0 0)))))

(defn- count-trees [zone-map path]
  (count
   (filter #(= :tree %)
           (map #(.getElement zone-map %) path))))

(defn- read-problem
  "Read the input as a raw list of fields"
  []
  (get-map
   (string/split-lines (slurp (io/resource "day3-input.txt")))))

(defn- trees-for-path [zmap move]
  (count-trees zmap (create-path zmap move)))

(defn- part-two [zmap]
  (let [moves '[(1 1) (1 3) (1 5) (1 7) (2 1)]]
            (apply * (map (partial trees-for-path zmap) moves))))

(defn day3
  "Print the solutions for day 3"
  []
  (println "*** Results for day3:")
  (let [data (read-problem)]
    (println "Trees encountered for the 1↓ 3→ move:" (trees-for-path data '(1 3)))
    (println "Second part result:" (part-two data))))
