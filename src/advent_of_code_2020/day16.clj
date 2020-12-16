(ns advent-of-code-2020.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- line-to-ticket [line]
  (mapv #(Integer/valueOf %) (string/split line #",")))

(defn- raw-range-extremes-to-set [string-min string-max]
  (let [min-value (Integer/valueOf string-min)
        max-value (Integer/valueOf string-max)]
    (into #{} (range min-value (inc max-value)))))

(def field-rule-regexp #"(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)")

(defn- line-to-rule [line]
  (let [[_ field smin1 smax1 smin2 smax2] (re-matches field-rule-regexp line)]
    [(keyword (string/replace field #" " "-"))
     (set/union (raw-range-extremes-to-set smin1 smax1)
                (raw-range-extremes-to-set smin2 smax2))]))

(defn- read-tickets-data
  ([]
   (read-tickets-data (io/resource "day16-input.txt")))
  ([location]
   (let [[raw-rules raw-mine raw-nearby] (string/split (slurp location) #"\n\n")]
     {:rules (into {}  (map line-to-rule (string/split-lines raw-rules)))
      :my-ticket (line-to-ticket (second (string/split-lines raw-mine)))
      :nearby-tickets (mapv line-to-ticket (rest (string/split-lines raw-nearby)))})))

(defn- transpose-row [row]
  (map vec (partition 1 row)))

(defn- create-empty-matrix [rows]
  (vec (take rows (repeat []))))

(defn- append-to-matrix [matrix column]
  (mapv #(vec (concat % %2)) matrix column))

(defn- transpose-tickets-data
  "Take the list of tickets in the ticket-data and returns a vector of ordered column data

  Assumes that all vectors in the input are of the same length."
  [mtickets]
  (reduce append-to-matrix
          (create-empty-matrix (count (first mtickets)))
          (map transpose-row mtickets)))

(defn- merge-valid-values-sets
  "Converts the :rules part of ticket-data in a single set with the union of all rules"
  [rules]
  (reduce set/union #{} (map second rules)))

(defn- invalid-fields-for-ticket [valid-values-set ticket]
  (filter (complement valid-values-set) ticket))

(defn- is-invalid-ticket?
  "Returns true if any of the ticket's field is not in the allowed values set"
  [valid-values-set ticket]
  (not (empty? (invalid-fields-for-ticket valid-values-set ticket))))

(defn- get-valid-tickets
  "Returns all tickets which doesn't have a field not matching any rule

  Here the \"valid\" is not intended in an absolute sense, as a ticket may
  not comply with all columns and still be valid by this classification.

  The problem description excludes this last scenario, though."
  [tickets-data]
  (let [valid-values-set (merge-valid-values-sets (:rules tickets-data))]
   (filter #(not (is-invalid-ticket? valid-values-set %)) (:nearby-tickets tickets-data))))

(defn- satisfies-rule? [tickets-column values-set]
  (every? values-set tickets-column))

(defn- compute-satisfied-rules [tickets-column rules]
  (filter identity
          (map #(satisfies-rule? tickets-column %) rules)))

(defn- compute-columns-matching-rule [transposed-tickets values-set]
  (let [indexed-satisfies-rule (map-indexed (fn [index column]
                                            (list index (satisfies-rule? column values-set)))
                                            transposed-tickets)]
    (->> indexed-satisfies-rule
         (filter second)
         (map first)
         (into #{}))))

(defn- classify-columns
  "Return a map or rules -> columns satisfying the rule"
  [transposed-tickets rules]  
  (into {}
        (map (fn [[name values-set]]
               (vector name (compute-columns-matching-rule transposed-tickets values-set))) rules)))

(defn- classify-tickets-data [tickets-data]
  (classify-columns (transpose-tickets-data (get-valid-tickets tickets-data))
                    (:rules tickets-data)))

(defn- remove-column-from-fields [rules column]
  (into {} (map (fn [[name columns-set :as entry]]
                  (if (contains? columns-set column)
                    (vector name (disj columns-set column))
                    entry)) rules)))

(defn- simplify-classification
  ([classification] (simplify-classification classification {}))
  ([classification column-to-field]
   (let [[key column-set :as determined-field] (first (drop-while #(not= (count (second %)) 1) classification))
         column (first column-set)]
     (if determined-field
       (recur (remove-column-from-fields (dissoc classification key) column)
              (assoc column-to-field column key))
       [classification column-to-field]))))

(defn- compute-field-for-column [tickets-data]
  (let [[unclassified column-to-field] (simplify-classification (classify-tickets-data tickets-data))]
    (if (not (empty? unclassified))
      (throw (UnsupportedOperationException. "Sorry, it seems that the basic recursive field-column matching doesnt' work on your input (which is strange, because it works on mine…)"))
      column-to-field)))

(defn- get-departure-fields-indices [field-for-column]
  {:post [(= (count %) 6)]}
  (map first (filter #(-> % second str (.startsWith ":departure-")) field-for-column)))

(defn- compute-departure-product [tickets-data]
  (let [mapping (compute-field-for-column tickets-data)
        departure-columns (get-departure-fields-indices mapping)]
    (apply * (map second (select-keys (:my-ticket tickets-data) departure-columns)))))

(defn- compute-scanning-error-rate [{rules :rules, nearby-tickets :nearby-tickets}]
  (let [get-invalid-fields-f (partial invalid-fields-for-ticket (merge-valid-values-sets rules))]
    (apply + (map #(apply + %) (map get-invalid-fields-f nearby-tickets)))))

(defn day16
  "Print the solutions for day 16"
  []
  (println "*** Results for day16:")
  (let [ticket-data (read-tickets-data)]
    (println "Scanning error rate:" (compute-scanning-error-rate ticket-data))
    (println "Departure fields products" (compute-departure-product ticket-data))))
