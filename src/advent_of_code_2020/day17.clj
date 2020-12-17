(ns advent-of-code-2020.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(defn- get-cubes-indices [line]
  (map first
       (filter (comp #{\#} second)
               (map-indexed #(vector % %2) line))))

(defn- lines-to-slice [lines]
  (into #{} (reduce concat '()
                    (map (fn [[row columns]]
                           (map #(vector row %) columns))
                         (map-indexed vector
                                      (map get-cubes-indices lines))))))

(defn- read-initial-slice
  ([] (read-initial-slice (io/resource "day17-input.txt")))
  ([location]
   (-> location
       slurp
       string/split-lines
       lines-to-slice)))

(def displacements-3D (for [i (range -1 2)
                     j (range -1 2)
                     k (range -1 2)]
                 (vector i j k)))

(def displacements-4D (for [i (range -1 2)
                     j (range -1 2)
                     k (range -1 2)
                     w (range -1 2)]
                 (vector i j k w)))

(defn- compute-range-of-interaction [displacements coordinate]
  (into #{} (map #(mapv + % coordinate)  displacements)))

(defn- should-turn-on? [occupied-neighbors]
  (= occupied-neighbors 3))

(defn- should-turn-off? [occupied-neighbors]
  (and (not= occupied-neighbors 2)
       (not= occupied-neighbors 3)))

(defn- evolve-place [displacements state coordinate]
  ;;; TODO/FIXME Convoluted, you can probably just account or the central place without removing it
  (let [place-status (state coordinate)
        interesting-neighbors (disj (compute-range-of-interaction displacements coordinate) coordinate)
        neighbors (count (filter identity (map state interesting-neighbors)))]
    (and
     (if place-status
       (not (should-turn-off? neighbors))
       (should-turn-on? neighbors))
     coordinate)))

(defn- compute-affected-squares [displacements state]
  (reduce set/union #{}  (map #(compute-range-of-interaction displacements %) state)))

(defn evolve [displacements state]
  (let [interesting-squares (compute-affected-squares displacements state)]
    (into #{} (filter identity (pmap (partial evolve-place displacements state) interesting-squares)))))

(defn- add-dimensions [initial-slice dimensions]
  (let [extra-dimensions (repeat dimensions 0)]
    (into #{}
          (map #(vec (concat % extra-dimensions)) initial-slice))))

(defn- state-cubes-after-bootstrap [displacements state]
  (count
   (last
    (take (inc 6) (iterate (partial evolve displacements) state)))))

(defn- part-2 [initial-slice]
  (state-cubes-after-bootstrap displacements-4D
                               (add-dimensions initial-slice 2)))

(defn- part-1 [initial-slice]
  (state-cubes-after-bootstrap displacements-3D
                               (add-dimensions initial-slice 1)))

(defn day17
  "Print the solution for day 17"
  []
  (println "*** Results for day17:")
  (let [initial-slice (read-initial-slice)]
    (println "Cubes after 6 cycles: "(part-1 initial-slice))
    (println "Hypercubes after 6 cycles: "(part-2 initial-slice))))
