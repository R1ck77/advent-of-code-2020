(ns advent-of-code-2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def problem-regexp #"([0-9]+)-([0-9]+) (.): ([a-z]+)")

(defn- read-fields [line]
  (let [groups (rest (re-matches problem-regexp line))]
    {:policy {:value1 (Integer/valueOf (first groups))
              :value2 (Integer/valueOf (second groups))
              :letter (first (nth groups 2))}
     :password (nth groups 3)}))

(defn- read-problem
  "Read the input as a raw list of fields"
  []
  (map read-fields (string/split-lines (slurp (io/resource "day2-input.txt")))))

(defn- is-password-valid-by-policy1? [{{min-rep :value1
                                        max-rep :value2
                                        letter :letter} :policy
                                       password :password}]
  (let [repetitions (count (filter #(= % letter) password))]
    (and (>= repetitions min-rep)
         (<= repetitions max-rep))))

(defn- xor
  "A very simple XOR operator. Why is this missing?"
  [a b]
  (and (or a b) (not (and a b))))

(defn- is-password-valid-by-policy2? [{{min-rep :value1
                                        max-rep :value2
                                        letter :letter} :policy
                                       password :password}]
  (xor (= (nth password (dec min-rep)) letter)
       (= (nth password (dec max-rep)) letter)))

(defn- count-valid [data policy-predicate]
  (count (filter policy-predicate data)))

(defn day2
  "Print the solutions for day 2"
  []
  (println "*** Results for day2:")
  (let [data (read-problem)]
    (println "Valid by policy 1: " (count-valid data is-password-valid-by-policy1?))
    (println "Valid by policy 2: " (count-valid data is-password-valid-by-policy2?))))
