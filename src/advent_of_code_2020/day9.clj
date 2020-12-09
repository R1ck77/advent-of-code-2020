(ns advent-of-code-2020.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code-2020.handheld :as handheld])
  (:import [advent_of_code_2020.handheld CpuState]))

(def seq-size 25)

(defn line-to-number [text]
  (Long/valueOf text))

(defn- read-numbers
  ([]
   (read-numbers (io/resource "day9-input.txt")))
  ([location]
   (mapv line-to-number
         (string/split-lines (slurp location)))))

(defn- valid-partition?
  "Returns true if the last number of the list is not the sum of two preceding numbers"
  [partition]
  (let [seed (take (dec (count partition)) partition)
        value (last partition)
        reduced-seed (filter #(<= % value) seed)]
    (some #(= (apply + %) value) (for [i reduced-seed, k reduced-seed] (list i k)))))

(defn- get-first-invalid-number [numbers]
  (-> (filter (complement valid-partition?) (partition (inc seq-size) 1 numbers))
      first
      last))

(defn- sub-sequences
  "From a sequence (i1 i2 i3 i4 ...) return a lazy sequence of ranges ((i1) (i1 i2) (i1 i2 i3) ...)"
  [numbers]
  (map #(take (inc %) numbers)
       (range (count numbers))))

(defn- sums-to-value? [value sub-sequence]
  (= value (apply + sub-sequence)))

;;; TODO/FIXME working, but ugly…
(defn- compute-sequential-partials [numbers value]
  (let [precursors (take-while #(not= % value) numbers)]
    (first
     (filter identity
             (pmap (fn [index]
                     (first
                      (filter (partial sums-to-value? value)
                              (sub-sequences (drop index precursors)))))
                   (range (count precursors)))))))

(defn- extremes-of-sequence-sum [numbers value]
  (let [sequence (compute-sequential-partials numbers value)]
    (+ (apply min sequence) (apply max sequence))))

(defn day9
  "Print the solutions for day 9"
  []
  (println "*** Results for day9:")
  (let [numbers (read-numbers)
        invalid-number (get-first-invalid-number numbers)]
    (println "First invalid number in the XMAS sequence:" invalid-number)
    (println "Sequence of partials of the invalid number:" (extremes-of-sequence-sum numbers invalid-number)))
  (shutdown-agents))
