(ns advent-of-code-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- line-to-jolt [text]
  (Long/valueOf text))

(defn- read-jolts
  ([]
   (read-jolts (io/resource "day10-input.txt")))
  ([location]
   (into #{} (map line-to-jolt
                  (string/split-lines (slurp location))))))

(defn- combine-differences [differences]
  (* (get differences 3)
     (get differences 1)))

(defn- prepare-data [jolts]
  (let [sorted (vec (conj (sort jolts) 0))
        device-joltage (+ 3 (last sorted))]
    (partition 2 1 (conj sorted device-joltage))))

(defn- compute-deltas [jolts]
  (map #(apply - (reverse %))
       (prepare-data jolts)))

(defn- compute-delta-products [jolts]
  (-> jolts
      compute-deltas
      frequencies
      combine-differences))

(defn- split-sequence
  "Return the valid subsequences of the current sequence"
  [jolts]
  (let [head (first jolts)
        remaining (rest jolts)]
    (->> (iterate rest remaining)
         (take 3)
         (filter (complement empty?))
         (filter #(<= (- (first %) head) 3)))))

(def ^:private count-3-2-1-subsequences!)

(defn- compute-partial!
  "Compute extra number of branches for the first argument, using the dictionary of pre-computed results.

  This function updates the dictionary during the subcomputations."
  [jolts solved-atom]
  (let [branches-count (->> (split-sequence jolts)
                            (map #(count-3-2-1-subsequences! 1 % solved-atom))
                            (apply +))]
   (dec branches-count)))

(defn- solve-partial!
  "Use dynamic programming to compute the number of subsequences.

  Updates the atom with all pre-computed subsequences (2-valued
  sequences are not cached as they translate directly to 0)."
  [jolts solved-atom]
  (if-let [partial (get @solved-atom jolts)]
    partial
    (let [new-partial (compute-partial! jolts solved-atom)]
      (swap! solved-atom #(assoc % jolts new-partial))
      new-partial)))

(defn- count-3-2-1-subsequences!
  "Count the number of \"max 3 jump\" sub-sequences for a sequence of sorted jolts"
  ([jolts]
   (count-3-2-1-subsequences! 1 jolts (atom {})))
  ([sum jolts partials-solved-atom]
   (cond
     (<= (count jolts) 2) sum
     :default (+ sum (solve-partial! jolts partials-solved-atom)))))

(defn- compute-combinations
  "Count the nuber of \"3 or less\" from 0 to the jolts collection"
  [jolts]
  (let [sorted-list (sort (conj jolts 0))]
    (count-3-2-1-subsequences! sorted-list)))

(defn day10
  "Print the solutions for day 10"
  []
  (println "*** Results for day10:")
  (let [jolts (read-jolts)]
    (println "1-jolt × 3-jolts differences:" (compute-delta-products jolts))
    (println "Chargers combinations:" (compute-combinations jolts))))
