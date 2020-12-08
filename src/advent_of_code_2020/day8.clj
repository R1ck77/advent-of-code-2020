(ns advent-of-code-2020.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code-2020.handheld :as handheld])
  (:import [advent_of_code_2020.handheld CpuState]))

(defn- line-to-opcode [line]
  [(keyword (.substring line 0 3))
   (Integer/valueOf (.substring line 4))])

(defn- read-opcodes
  "Read the list of instructions as a [opcode argument].

  Opcode can be :jmp :acc :nop, argument is an integer"
  ([]
   (read-opcodes (io/resource "day8-input.txt")))
  ([location]
   (mapv line-to-opcode
         (string/split-lines (slurp location)))))

(defprotocol SimulationState
  (getCpuState [this] "Returns the current cpu state")
  (getVisited [this] "Returns the set of instructions already visited")
  (isLooping [this] "Returns true if the next instruction to be executed has been already visited")
  (isTerminated [this] "Returns true if the IP goes out of the program bounds")
  (step [this] "Step the simulation, returning a new simulation"))

(def ^:private create-simulation)

(defn- step-simulation [cpu-state visited]
  (let [next-visited-ip (.getIP cpu-state)
        next-cpu-state (.evolve cpu-state)]
    (create-simulation next-cpu-state
                       (conj visited next-visited-ip))))

(defn create-simulation
  ([cpu-state]
   (create-simulation cpu-state #{}))
  ([cpu-state visited]
   (reify SimulationState
     (getCpuState [this] cpu-state)
     (getVisited [this] visited)
     (isLooping [this]
       (visited (.getIP cpu-state)))
     (isTerminated [this]
       (>= (.getIP cpu-state) (count (.getProgram cpu-state))))
     (step [this]
       (step-simulation cpu-state visited)))))

(defn- create-infinite-simulation [opcodes]
  (iterate #(.step %)
           (create-simulation (handheld/create-cpu-state opcodes))))

(defn- compute-accumulator-before-loop-or-stop [opcodes]
  (-> (drop-while #(and (not (.isLooping %)) (not (.isTerminated %)))
                  (create-infinite-simulation opcodes))
      first
      .getCpuState
      .getAcc))

(defn- program-loops?
  [opcodes]
  (-> (drop-while #(and (not (.isLooping %)) (not (.isTerminated %)))
                  (create-infinite-simulation opcodes))
      first
      .isLooping))

(defn- mutation-indices
  "Return all indices where the program can be mutated from nop to jmp or the other way around"
  [opcodes]
  (filter #(#{:nop, :jmp} (first (nth opcodes %)))
          (range (count opcodes))))

(defn- switch-opcode [[op number]]
  [({:nop :jmp, :jmp :nop} op) number])

(defn- mutate-at
  "Return a program that is mutated at instruction index

  The opcode at the index must either be :nop or :jmp"
  [opcodes index]
  (assoc opcodes index (switch-opcode (nth opcodes index))))

(defn- mutated-programs
  "Returns an ordered lazy sequence of mutated programs"
  [opcodes]
  (map (partial mutate-at opcodes) (mutation-indices opcodes)))

(defn- first-terminating-program
  "Return the first program that does not loop"
  [opcodes]
  (first (filter (complement program-loops?) (mutated-programs opcodes))))

(defn day8
  "Print the solutions for day 8"
  []
  (println "*** Results for day8:")
  (let [opcodes (read-opcodes)]
    (println "Accumulator before looping:" (compute-accumulator-before-loop-or-stop opcodes))
    (println "Accumulator after the first non-looping program:" (compute-accumulator-before-loop-or-stop (first-terminating-program opcodes)))))
