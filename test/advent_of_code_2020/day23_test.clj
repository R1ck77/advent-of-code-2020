(ns advent-of-code-2020.day23-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day23 :as day23]))

(def a-sample [3 8 9 1 2 5 4 6 7])

(deftest test-day23-part-1
  (testing "golden master (move)"
    (is (= [2 8 9 1 5 4 6 7 3]
           (.getBuffer (day23/move (day23/create-buffer a-sample))))))
  (testing "golden master (moves)"
    (is (= [8 3 7 4 1 9 2 6 5]
           (.getBuffer (day23/moves (day23/create-buffer a-sample) 10)))))
  (testing "golden master (1k moves"
    (is (= [8 9 2 5 6 4 7 3 1]
           (.getBuffer (day23/moves (day23/create-buffer a-sample) 1000)))))
  (testing "golden master (score)"
    (is (= "6489102573" (day23/part-1 (vec (range 1 11)))))))
