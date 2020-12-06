(ns advent-of-code-2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- blob-to-group [text]
  (map #(into #{} %) (string/split text #"\n")))

(defn- read-groups
  "Read the input as a list of groups, each containing a vector of sets"
  []
  (map blob-to-group
       (string/split (slurp (io/resource "day6-input.txt"))
                     #"[\n]{2}")))

(defn- sum-combined-groups [groups f]
  (apply +
         (map count
              (map #(apply f %) groups))))

(defn- compute-union-sum [problem]
  (sum-combined-groups problem set/union))

(defn- compute-intersection-sum [problem]
  (sum-combined-groups problem set/intersection))

(defn day6
  "Print the solutions for day 6"
  []
  (println "*** Results for day6:")
  (let [groups (read-groups)]
    (println "Sum of all choices (union):" (compute-union-sum groups))
    (println "Sum of all choices (intersection):" (compute-intersection-sum groups))))
