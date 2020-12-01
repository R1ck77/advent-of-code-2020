(ns advent-of-code-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- pick-solution
  "Pick the first term whose elements sum is 2020"
  [xterms]
  (first
   (filter #(= (apply + %) 2020) xterms)))

(defn- solve-for-n-tuples
  "Return the product of the terms of the first element whose sum is 2020"
  [xtuples]
  (apply * (pick-solution xtuples)))

(defn- three-ways-diagonal-cartesian
  "Return a lazy sequence of triplets (i j k) with i,j,k in xint, and i < j < k"
  [xint]
  (filter #(and (< (first %) (second %))
                (< (second %) (nth % 2)))
          (for [i xint j xint k xint] [i j k])))

(defn- find-triplet
  "Return the solution for triplets"
  [xint]
  (solve-for-n-tuples
   (three-ways-diagonal-cartesian xint)))

(defn- two-ways-diagonal-cartesian
  "Return a lazy sequence of pairs (i j) with i,j in xint, and i < j"
  [xint]
  (filter #(< (first %) (second %))
                  (for [i xint j xint] [i j])))

(defn- find-pair
  "Return the solution for pairs"
  [xint]
  (solve-for-n-tuples
   (two-ways-diagonal-cartesian xint)))

(defn- read-problem
  "Read the list of numbers of the problem"
  []
  (map #(Integer. %)
       (string/split-lines (slurp (io/resource "day1-input.txt")))))

(defn day1
  "Print the solutions for day one"
  []
  (println "*** Results for day1:")
  (println "Pair: " (find-pair (read-problem)))
  (println "Triplet: " (find-triplet (read-problem))))



