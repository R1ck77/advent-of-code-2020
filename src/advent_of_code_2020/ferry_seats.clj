(ns advent-of-code-2020.ferry-seats
  (:require [clojure.string :as string]))

(def floor-kwd :floor)
(def empty-seat-kwd :empty)
(def taken-seat-kwd :taken)

(defprotocol SeatsLayout
  (getSize [this])
  (getPos [this position])
  (getSeatsAroundPosition [this position])
  (evolve [this evolve-f])
  ;; TODO/FIXME This one breaks encapsulation :(
  (getRawLayout [this])
  (countOccupiedSeats [this]))

(def ^:private keyword-to-string {taken-seat-kwd \#
                                  empty-seat-kwd \L
                                  floor-kwd \.})

(defn- count-occupied-places [row]
  (count (filter #{taken-seat-kwd} row)))

(defn- raw-layout-to-string [layout]
  (string/join "\n" (map #(apply str (map keyword-to-string %)) layout)))

(defn- is-valid-coordinate [[rows columns] [row column]]
  (and (>= row 0)
       (>= column 0)
       (< row rows)
       (< column columns)))

(defn- get-range [bounds position delta]
  (take-while #(is-valid-coordinate bounds %)
              (drop 1 (iterate #(mapv + % delta) position))))

(defn- get-seats-ranges
  "Returns lazy lists of neighbors"
  [layout position neighbors-ranges-f]
  (map (fn [range]
         (map #(get-in layout %) range))
       (neighbors-ranges-f position)))

(def ^:private position-shifts [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]])


(defn- get-neighbors-ranges [bounds position]
  (filter (complement empty?)
          (map #(get-range bounds position %)
               position-shifts)))

(defn- get-size
  [layout]
  (vector (count layout) (count (first layout))))

(defn create-seats-layout
  ([raw-layout]
   (create-seats-layout raw-layout
                        (memoize
                         (partial get-neighbors-ranges
                                  (get-size raw-layout)))))
  ([raw-layout get-neighbors-f]
   (let [size (get-size raw-layout)]
     (reify SeatsLayout
       (toString [this]
         (raw-layout-to-string raw-layout))
       (getSize [this] size)
       (getPos [this position]
         (get-in raw-layout position))
       (getRawLayout [this] raw-layout)
       (getSeatsAroundPosition [this position]
         (get-seats-ranges raw-layout position get-neighbors-f))
       (evolve [this evolve-f]
         (create-seats-layout (evolve-f this) get-neighbors-f))
       (countOccupiedSeats [this]
           (->> raw-layout
                (map count-occupied-places)
                (apply +)))))))
