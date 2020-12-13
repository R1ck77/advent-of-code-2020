(ns advent-of-code-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- read-id-list [raw-line]
  (mapv #(if (= % "x")
           nil
           (Integer/valueOf %))
        (string/split raw-line #",")))

(defn- read-schedule
  ([]
   (read-schedule (io/resource "day13-input.txt")))
  ([location]
   (let [lines (string/split-lines (slurp location))]
     {:time (Integer/valueOf (first lines))
      :id-list (read-id-list (second lines))})))

(defn- compute-indexed-remainders [value indexed-factors]
  (map (fn [[index factor]]
         (vector index (mod value factor)))
       indexed-factors))

(defn- compute-wait-time [[i u] [j v]]
  {:pre [(= i j)]}
  [i (- v u)])

(defn- compute-indexed-factors [id-list]
  (filter (comp identity second)
          (map-indexed vector id-list)))

(defn- compute-wait-times [{time :time, id-list :id-list}]
  (let [explicit-indexed-id  (compute-indexed-factors id-list)
        remainders (compute-indexed-remainders time explicit-indexed-id)]
    (map compute-wait-time remainders explicit-indexed-id)))

(defn- compute-first-id [{id-list :id-list :as schedule}]
  (let [wait-times (compute-wait-times schedule)
        [index minimum-wait] (first (sort-by second wait-times))]
    (* minimum-wait (nth id-list index))))

(defn- compute-special-coincidence-brute-force
  "Brute force approach to the part 2

  Obviously too slow to converge in reasonable time even for tame inputs like:

  [1789 37 47 1889] ⇒ 1202161486

  I'm keeping it as a curiosity"
  [id-list]
  (let [indexed-factors (compute-indexed-factors id-list)]
    (first (reduce (fn [prev-range [dx factor]]
                     (filter #(zero? (mod (+ % dx) factor)) prev-range))
                   (range) indexed-factors))))

(defn- compute-next-coincidence-and-period
  "Given the 'coincidence' value for the previous terms (and its period), return the next one accounting for an extra term.

  Say that for the first n terms the problem solution is x=V (with period P), finding the solution
  for the n + 1 term (named 'id', which has to be 0 with a displacement 'dx') I need to find a k,
  such that:
  
  ((k × P + V) + dx) mod id = 0

  The solution for the first n+1 terms is (k × P + V).

  The period is simply (P × id) under the assumption that no term shares common factors (which should be the case)."
  [[prev-coincidence period] [next-dx next-value]]
  (let [next-coincidence (first
                          (filter #(zero? (mod (+ % next-dx) next-value))
                                  (map #(+ prev-coincidence (* period %)) (range))))
        next-period (* period next-value)]
    [next-coincidence next-period]))

(defn- compute-special-coincidence
  "Recursively compute the special coincidence that's the solution to part 2.

  This one took me a *lot* of pen and paper :/"
  [id-list]
  (let [initial-factors (compute-indexed-factors id-list)]
    (loop [initial-state (first initial-factors)
           remaining (rest initial-factors)]
      ;;(println "State: " initial-state)
      (if (empty? remaining)
        (first initial-state)
        (let [next-initial-state (compute-next-coincidence-and-period initial-state (first remaining))]
          (recur next-initial-state (rest remaining)))))))

(defn day13
  "Print the solutions for day 13"
  []
  (println "*** Results for day13:")
  (let [schedule (read-schedule)]
    (println "First bus ID available:" (compute-first-id schedule))
    (println "Special coincidence:" (compute-special-coincidence (:id-list schedule)))))
