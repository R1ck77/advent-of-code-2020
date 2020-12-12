(ns advent-of-code-2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code-2020.ferry-seats :as ferry-seats :refer [floor-kwd empty-seat-kwd taken-seat-kwd]]))

(def char-to-position {\. floor-kwd
                       \L empty-seat-kwd})

(defn- line-to-row [line]
  {:pre [(= -1 (.indexOf line "#"))]}
  (mapv char-to-position line))

(defn- read-layout
  ([]
   (read-layout (io/resource "day11-input.txt")))
  ([location]
   (ferry-seats/create-seats-layout
    (mapv line-to-row
          (string/split-lines (slurp location))))))

(defn- get-neighbors [layout position selector-f]
  (map selector-f (.getSeatsAroundPosition layout position)))

(defn- should-get-empty? [threshold neighbors]
  (>= (count (filter #{taken-seat-kwd} neighbors)) threshold))

(defn- evolve-taken [layout position selector-f threshold]
  (if (should-get-empty? threshold (get-neighbors layout position selector-f))
    empty-seat-kwd
    taken-seat-kwd))

(defn- should-be-taken? [neighbors]
  (every? #{empty-seat-kwd floor-kwd} neighbors))

(defn- evolve-empty [layout position selector-f]
  (if (should-be-taken? (get-neighbors layout position selector-f))
    taken-seat-kwd
    empty-seat-kwd))

(defn- get-next-state
  "Return the next state for a specific position"
  [layout selector-f threshold [row column :as position]]
  (let [current (.getPos layout position)]
    (cond
      (= current empty-seat-kwd) (evolve-empty layout position selector-f)
      (= current taken-seat-kwd) (evolve-taken layout position selector-f threshold)
      :default current)))

(defn- evolve-layout [old-layout selector-f threshold]
  (let [[rows columns] (.getSize old-layout)]
    (vec
     (for [row (range rows)]
       (mapv #(get-next-state old-layout selector-f threshold %)
             (mapv #(vector row %) (range columns)))))))

(defn- evolve [layout selector-f threshold]
  (.evolve layout #(evolve-layout % selector-f threshold)))

(defn- evolve-to-equilibrium [layout selector-f threshold]
  (loop [old-layout layout]
    (let [new-layout (evolve old-layout selector-f threshold)]
      (if (= (.getRawLayout new-layout)
             (.getRawLayout old-layout))
        new-layout
        (recur new-layout)))))

(defn- first-in-sight [neighbors]
  (or (first (drop-while #{ferry-seats/floor-kwd} neighbors))
      ferry-seats/floor-kwd))

(defn- compute-occupied-seats-at-equilibrium [layout selector-f threshold]
  (.countOccupiedSeats (evolve-to-equilibrium layout selector-f threshold)))

(defn day11
  "Print the solutions for day 11"
  []
  (println "*** Results for day11:")
  (let [layout (read-layout)]
    (println "Occupied seats at equilibrium (first neighbors rule):"
             (compute-occupied-seats-at-equilibrium layout first 4))
    (println "Occupied seats at equilibrium (first in sight rule):"
             (compute-occupied-seats-at-equilibrium layout first-in-sight 5))))
