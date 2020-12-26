(ns advent-of-code-2020.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import [java.util Deque List LinkedList]))

(set! *warn-on-reflection* true)
(set! *assert* false) ; hardly makes a difference

(defn- read-problem
  ([] (read-problem (io/resource "day23-input.txt")))
  ([location]
   (let [codes (string/split (string/replace(slurp location) #"[^0-9]+" "") #"")]
     (mapv #(Integer/valueOf ^String %) codes))))

(defprotocol CircularBuffer
  (getCupsAfter1 [this])
  (move [this])
  (getBuffer [this]))

(defn- as-list [data current]
  (vec (take (count data)
         (iterate #(get data %) current))))

(defn- update-current! [current-atom data-atom]
  {:pre [(number? @current-atom)]
   :post [(number? @current-atom)]}
  (swap! current-atom (fn [current]
                        (get @data-atom current))))

(defn- insert-after-destination! [data-atom destination removed]
  {:pre [(number? destination) (vector? removed)]}
  (let [data @data-atom
        second-seam (get data destination)
        [el1 el2 el3] removed]
    (swap! data-atom (fn [data]
                       (assoc! (assoc! (assoc! (assoc! data
                                                destination el1)
                                        el1 el2)
                                       el2 el3)
                               el3 second-seam)))))

(defn- search-destination [current max-value data]
  {:pre [(integer? current)]}
  (let [starting-point (if (= current 1) max-value (dec current))]
    (first (drop-while #(not (contains? data %)) (iterate #(mod (dec %) (inc max-value)) starting-point)))))

(defn- remove-next-3-clockwise! [current data-atom]
  {:pre [(integer? current) (instance? clojure.lang.Atom data-atom)]}
  (let [data @data-atom
        el1 (get data current)
        el2 (get data el1)
        el3 (get data el2)
        seam (get data el3)
        new-data (assoc! (dissoc! data el1 el2 el3)
                         current seam)]
    (reset! data-atom new-data)
    [el1 el2 el3]))

(defn create-buffer
  "Create a roughly optimized CircularBuffer implementation"
  [numbers]
  (let [current-atom (atom (first numbers))
        data-atom  (atom (transient (into {} (map vec (conj (partition 2 1 numbers) (list (last numbers) (first numbers)))))))
        max-value (apply max numbers)]
    (reify CircularBuffer
      (getCupsAfter1 [this]
        (take (dec (count @data-atom)) (iterate #(get @data-atom %) (get @data-atom 1))))
      (move [this]
        (let [removed (remove-next-3-clockwise! @current-atom data-atom)
              destination (search-destination @current-atom max-value @data-atom)]
          (insert-after-destination! data-atom destination removed)
          (update-current! current-atom data-atom)
          this))
      (getBuffer [this]
        (as-list @data-atom @current-atom)))))

(defn moves [circular-buffer times]
  (first (reverse (take (inc times) (iterate #(.move ^advent_of_code_2020.day23.CircularBuffer %) circular-buffer)))))

(defn create-cups-state
  [values]
  {:cups (doto (LinkedList.)
           (.addAll values))
   :max-value (apply max values)
   :current 0})

(defn part-1 [cups]
  (apply str (.getCupsAfter1 ^advent_of_code_2020.day23.CircularBuffer (moves (create-buffer cups) 100))))

(defn- expand-sequence
  ([cups] (expand-sequence cups 1000000))
  ([cups total]
   (vec (concat cups (range (inc (apply max cups)) (inc total))))))

(defn- get-product-of-2-cups-after-1 [buffer]
  (apply * (take 2 (.getCupsAfter1 ^advent_of_code_2020.day23.CircularBuffer buffer))))

(defn part-2 [cups]
  (get-product-of-2-cups-after-1 (moves (create-buffer (expand-sequence cups)) 10000000)))

(defn day23
  "Print the solution for day 23.

  I wasted literally *days* looking for the clever mathematical trick
  to would speed up the problem - under the (wrong) assumtion that no
  data structure would have been fast enough to do the 10M recursions
  in reasonable time - only to find out that the problems was order of
  magnitues dumber and straightforward than I expected it to be.

  'Insanity is doing the same thing over and over and over again, and
  expecting the result to change.'
  [Misattributed to Einstein]"
  []
  (println "*** Results for day23:")
  (let [cups (read-problem)]
    (println "Labels on the cups (from 1, excluded):" (part-1 cups))
    (println "Product of the 2 cups next to 1:" (part-2 cups))))
