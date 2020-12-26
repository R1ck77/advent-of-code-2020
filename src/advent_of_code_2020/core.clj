(ns advent-of-code-2020.core
  (:require [advent-of-code-2020.day1 :as day1]
            [advent-of-code-2020.day2 :as day2]
            [advent-of-code-2020.day3 :as day3]
            [advent-of-code-2020.day4 :as day4]
            [advent-of-code-2020.day5 :as day5]
            [advent-of-code-2020.day6 :as day6]
            [advent-of-code-2020.day7 :as day7]
            [advent-of-code-2020.day8 :as day8]
            [advent-of-code-2020.day9 :as day9]
            [advent-of-code-2020.day10 :as day10]
            [advent-of-code-2020.day11 :as day11]
            [advent-of-code-2020.day12 :as day12]
            [advent-of-code-2020.day13 :as day13]
            [advent-of-code-2020.day14 :as day14]
            [advent-of-code-2020.day15 :as day15]
            [advent-of-code-2020.day16 :as day16]
            [advent-of-code-2020.day17 :as day17]
            [advent-of-code-2020.day18 :as day18]
            [advent-of-code-2020.day19 :as day19]
            [advent-of-code-2020.day20 :as day20]
            [advent-of-code-2020.day21 :as day21]
            [advent-of-code-2020.day22 :as day22]
            [advent-of-code-2020.day23 :as day23]
            [advent-of-code-2020.day24 :as day24]))

(def days [(fn [] (println "Days are 1-based!"))
           day1/day1
           day2/day2
           day3/day3
           day4/day4
           day5/day5
           day6/day6
           day7/day7
           day8/day8
           day9/day9
           day10/day10
           day11/day11
           day12/day12
           day13/day13
           day14/day14
           day15/day15
           day16/day16
           day17/day17
           day18/day18
           day19/day19
           day20/day20
           day21/day21
           day22/day22
           day23/day23
           day24/day24])

(defn advent-of-code-2020 [& args]
  (if (not args)
    (dorun (map #(%) (rest days)))
    (try
      ((nth days (Integer/valueOf (first args))))
      (catch Exception e (println (format "Unable to execute day '%s': %s" (first args) e)))))
  (shutdown-agents))



