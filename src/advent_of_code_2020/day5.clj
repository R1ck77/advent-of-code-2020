(ns advent-of-code-2020.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def row-mask '(64 32 16 8 4 2 1))
(def column-mask '(4 2 1))

(defn- read-problem
  "Read the input as a list of raw passport definitions"
  []
  (string/split-lines
   (slurp (io/resource "day5-input.txt"))))

(defn- compute-row [raw-pass]
  (apply + (map * row-mask (map {\F 0, \B 1 }  (take 7 raw-pass)))))

(defn- compute-column [raw-pass]
  (apply + (map * column-mask (map {\R 1, \L 0} (drop 7 raw-pass)))))

(defn- raw-pass-to-pos
  "Convert the raw list of letters into a row/column dictionary"
  [raw-pass]
  (let [binary-row (map {\F 0, \B 1} (take 7))]
    {:row (compute-row raw-pass)
     :column (compute-column raw-pass)}))

(defn- compute-boarding-ID
  "Convert "
  [{row :row, column :column}] 
  (+ (* 8 row) column))

(defn- compute-boarding-IDs [raw-passes]
  (map compute-boarding-ID
       (map raw-pass-to-pos raw-passes)))

(defn- compute-maximum-id [raw-passes]
  (apply max (compute-boarding-IDs raw-passes)))

(defn- is-expected-id? [id-set id]
  (and (not (id-set id))
       (id-set (inc id))
       (id-set (dec id))))

(defn- find-empty-seat
  "Find the missing ID x for which x+1 and x-1 are in the list"
  [xid]
  (let [id-set (into #{} xid)]
    (first
     (filter (partial is-expected-id? id-set) (map inc xid)))))

(defn day5
  "Print the solutions for day 5"
  []
  (println "*** Results for day5:")
  (let [raw-passes (read-problem)]
    (println "Highest boarding pass ID:" (compute-maximum-id raw-passes))
    (println "Missing ID sandwiched between occupied IDs: " (find-empty-seat (compute-boarding-IDs raw-passes)))))
