(ns advent-of-code-2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- line-to-direction [text]
  (vector (keyword (.substring text 0 1))
          (Integer/valueOf (.substring text 1))))

(defn- read-directions
  ([]
   (read-directions (io/resource "day12-input.txt")))
  ([location]
   (mapv line-to-direction
         (string/split-lines (slurp location)))))

(defprotocol State
  (updateState [this direction])
  (manhattan [this]))

(defn- compute-new-direction [[east north :as direction] value]
  (let [rotation (mod value 360)]
    (case rotation
      0 direction
      90 [north (- east)]
      180 (mapv - direction) 
      270 [(- north) east]
      (throw (IllegalArgumentException. (format "Invalid angle value (%d → %d)" value rotation))))))

(defn- compute-manhattan [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn- forward-by-waypoint [state value]
  (let [displacement (map #(* value %) (:waypoint state))]
   (update state :position #(map + % displacement))))

(defn- move-waypoint [state value]
  (update state :waypoint #(map + % value)))

(defn- turn-waypoint [state value]
  (update state :waypoint #(compute-new-direction % value)))

(defn- update-waypoint-state [state [opcode value]]
  (case opcode
    :L (turn-waypoint state (- value))
    :R (turn-waypoint state value)
    :N (move-waypoint state [0 value])
    :S (move-waypoint state [0 (- value)])
    :E (move-waypoint state [value 0])
    :W (move-waypoint state [(- value) 0])
    :F (forward-by-waypoint state value)
    (throw (IllegalArgumentException. (format "Unexpected direction (%s)!" opcode)))))

(defn create-waypoint-state
  ([]
   (create-waypoint-state {:position [0 0]
                           :waypoint [10 1]}))
  ([state]
   (reify State
     (manhattan [this]
       (compute-manhattan (:position state)))
     (updateState [this direction]
       (create-waypoint-state
        (update-waypoint-state state direction))))))

(defn- turn [state value]
  (update state :direction #(compute-new-direction % value)))

(defn- move [state value]
  (update state :position #(mapv + % value)))

(defn- forward [{position :position, direction :direction} value]
  {:position (mapv + position (map #(* value %) direction))
   :direction direction})

(defn- update-simple-state [state [opcode value]]
  (case opcode
    :L (turn state (- value))
    :R (turn state value)
    :N (move state [0 value])
    :S (move state [0 (- value)])
    :E (move state [value 0])
    :W (move state [(- value) 0])
    :F (forward state value)
    (throw (IllegalArgumentException. (format "Unexpected direction (%s)!" opcode)))))

(defn create-simple-state
  ([]
   (create-simple-state {:direction [1 0]
                         :position [0 0]}))
  ([state]
   (reify State
     (manhattan [this]
       (compute-manhattan (:position state)))
     (updateState [this direction]
       (create-simple-state (update-simple-state state direction))))))

(defn- compute-manhattan-distance
  [directions initial-state]
  (.manhattan
   (reduce #(.updateState % %2)
           initial-state
           directions)))

(defn day12
  "Print the solutions for day 12"
  []
  (println "*** Results for day12:")
  (let [directions (read-directions)]
    (println "Manhattan distance (basic directions):"
             (compute-manhattan-distance directions (create-simple-state)))
    (println "Manhattan distance (waypoint directions):"
             (compute-manhattan-distance directions (create-waypoint-state)))))
