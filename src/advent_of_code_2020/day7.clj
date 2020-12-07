(ns advent-of-code-2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- type-to-keyword [raw-type]
  (keyword (string/replace raw-type " " "-")))

(defn- get-object [raw-object-definition]
  (let [[_ raw-number raw-type] (re-matches #"([0-9]+) (.*) bag.*" raw-object-definition)]
    [(type-to-keyword raw-type) (Integer/valueOf raw-number)]))

(defn- split-objects [raw-objects]
  (string/split raw-objects #", "))

(defn- read-objects [raw-objects]
  (if (= raw-objects "no other bags.")
    {}
    (into {} (map get-object (split-objects raw-objects)))))

(defn- line-to-rule [line]
  (let [[_ raw-subject raw-object] (re-matches #"(.*) bags contain (no other bags.|.*)" line)]
    [(type-to-keyword raw-subject) (read-objects raw-object)]))

(defn- read-rules
  "Read the list of rules in the form {:type {contents}}, where in contents keys are the types and the values are the numbers

  Each type is codified as a keyword, like :light-gray (regardless of the multiplicity)"
  []
  (into {} (map line-to-rule
                (string/split-lines (slurp (io/resource "day7-input.txt"))))))

(defn- direct-container?
  "Returns the rule if the rule states that any of the targets is contained"
  [rule targets-set]
  (if (some #(targets-set (first %)) (second rule))
    rule))

(defn- get-containers-of
  "Returns the set of bag types that contain the selected types"
  [rules types-set]
  (let [container-types (map first
                             (filter identity
                                     (map #(direct-container? % types-set)  rules)))]
    (into #{} container-types)))

(defn- get-indirect-containers-of
  "Get the list of containers that contain (directly or indirectly) any of the targets"
  [rules targets-set]
  (loop [containers '()
         current-targets-set targets-set]
    (let [result (get-containers-of rules current-targets-set)
          new-containers (conj containers result)]
      (if (empty? result)
        (into #{} (apply concat new-containers))
        (recur new-containers result)))))

(defn- compute-shiny-gold-containers [rules]
  (count (get-indirect-containers-of rules #{:shiny-gold})))

(defn- count-contents-including-self
  "Return all bags contained in the target bag, counting the container itself.

  Recursive function that may blow if the nesting is too high"
  [rules target]
  (let [target-contents (get rules target)]
    (inc
     (apply + (map (fn [[bag-type number]]
                     (* (count-contents-including-self rules bag-type)
                        number))
                   target-contents)))))

(defn- count-contents
  "Return all bags contained in the target bag"
  [rules target]
  (dec (count-contents-including-self rules target)))

(defn day7
  "Print the solutions for day 7"
  []
  (println "*** Results for day7:")
  (let [rules (read-rules)]
    (println "Possible shiny gold bag containers:" (compute-shiny-gold-containers rules))
    (println "Bags contained in a single shiny gold bag:" (count-contents rules :shiny-gold))))
