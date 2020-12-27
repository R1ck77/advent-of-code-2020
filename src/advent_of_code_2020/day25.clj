(ns advent-of-code-2020.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)
(set! *assert* true)

(def ^:private module "Dividend to be used in all transformations" 20201227)
(def ^:private base-subject "Initial subject number used to create the public keys" 7)

(defn- read-problem
  ([] (read-problem (io/resource "day25-input.txt")))
  ([location]
   (mapv #(Integer/valueOf ^String %)
         (string/split-lines (slurp location)))))

(defn- do-loop
  ([subject] (do-loop 1 subject))
  ([previous subject]
   (mod (* previous subject) module)))

(defn loop-sequence
  "Return a sequence, starting with loop 1 (the seed, for low values)"
  [subject]
  (rest (iterate #(do-loop subject %) 1)))

(defn crack-loops
  ([key1 key2] (crack-loops 7 key1 key2))
  ([subject key1 key2]
   (loop [codes-seen (transient {})
          iteration 1
          prev (Long/valueOf 1)]
     (let [new-code (do-loop prev subject)
           new-codes (assoc! codes-seen new-code iteration)]
       (if (and (contains? new-codes key1)
                (contains? new-codes key2))
         (vector (get new-codes key1)
                 (get new-codes key2))
         (recur new-codes (inc iteration) new-code))))))

(defn loop-transform [value loop]
  (last (take loop (loop-sequence value))))

(defn compute-encryption-key [keys]
  (let [[loop1 loop2] (apply crack-loops keys)
        value-from-1 (future (loop-transform (second keys) loop1))
        value-from-2 (future (loop-transform (first keys) loop2))]
    (assert (= @value-from-1 @value-from-2))
    @value-from-1))

(defn- part-1
  "This was too easy: ominous…"
  [problem]
  (compute-encryption-key problem))

(defn day25
  "Print the solution for day 25"
  []
  (println "*** Results for day25:")
  (let [problem (read-problem)]
    (println "Encryption key:" (part-1 problem))))
