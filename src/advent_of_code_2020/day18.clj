(ns advent-of-code-2020.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn- line-to-expression [text]
  (read-string (str "(" text ")")))

(defn- read-expressions
  ([] (read-expressions (io/resource "day18-input.txt")))
  ([location]
   (->> location
       slurp
       string/split-lines
       (map line-to-expression))))

(def ^:private resolve-value)

(defn- resolve-expression [expr]
  (case (count expr)
    0 (throw (IllegalStateException. "Empty expression!"))
    1 (resolve-value (first expr))
    2 (throw (IllegalStateException. "Unexpected number of items (2) in an expression"))
    3 (let [result (list (second expr)
                          (resolve-value (first expr))
                          (resolve-value (nth expr 2)))]
        (eval result))
    (let [[start end] (split-at 3 expr)]
      (resolve-expression (conj end (resolve-expression start))))))

(defn- resolve-value [value]
  (cond
    (int? value) value
    (seq? value) (resolve-expression value)
    :default value))

(defn- contains-sums?
  "Returns false if the expression doesn't contain a '+ sign"
  [expression]
  (some #{'+} expression))

(defn- first-sum-index
  [expression]
  "Return the index of the first '+ sign in the expression"
  (->> expression
       (map-indexed vector)
       (filter (comp #{'+} second))
       first
       first))

(defn- wrap-first-summation
  "Wrap the first sum in the expression in parentheses"
  [expression]
  (let [index (first-sum-index expression)
        next-head (take (- index 1) expression)
        next-tail (drop (+ index 2) expression)
        parenthesized-terms (list (nth expression (dec index)) '+ (nth expression (inc index)))]
       (concat next-head
               (list parenthesized-terms)
               next-tail)))

(defn- wrap-summations
  "Wrap the sums in the expression in parentheses"
  [expression]
  (if (contains-sums? expression)
    (recur (wrap-first-summation expression))
    expression))

(defn- parenthesize
  "Take an expression and return one with explicit operator precedence"
  [expression]
  (let [expr (map (fn [term]
                    (if (seq? term)
                      (parenthesize term)
                      term)) expression)]
    (wrap-summations expr)))

(defn- part-2 [expressions]
  (apply + (map resolve-expression (map parenthesize expressions))))

(defn- part-1 [expressions]
  (apply + (map resolve-expression expressions)))

(defn day18
  "Print the solution for day 18"
  []
  (println "*** Results for day18:")
  (let [expressions-list (read-expressions)]
    (println "Sum of all expressions (flat precedence): "(part-1 expressions-list))
    (println "Sum of all expressions (alt precedence): "(part-2 expressions-list))))
