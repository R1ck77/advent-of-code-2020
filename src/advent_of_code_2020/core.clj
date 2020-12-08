(ns advent-of-code-2020.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code-2020.day1 :as day1]
            [advent-of-code-2020.day2 :as day2]
            [advent-of-code-2020.day3 :as day3]
            [advent-of-code-2020.day4 :as day4]
            [advent-of-code-2020.day5 :as day5]
            [advent-of-code-2020.day6 :as day6]
            [advent-of-code-2020.day7 :as day7]
            [advent-of-code-2020.day8 :as day8]))

(defn advent-of-code-2020 []
  (day1/day1)
  (day2/day2)
  (day3/day3)
  (day4/day4)
  (day5/day5)
  (day6/day6)
  (day7/day7)
  (day8/day8))



