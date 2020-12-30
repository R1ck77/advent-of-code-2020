(ns advent-of-code-2020.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(set! *warn-on-reflection* true)

(defn- read-rule [^String text]
  (cond
    (.startsWith text "\"") (.substring text 1 2)
    (.contains text "|") (into #{}  (map read-rule (string/split text #" [|] ")))    
    :default (read-string (format "[%s]" text))))

(defn- read-rules [text]
  (let [fields (map #(string/split % #": ")
                    (string/split-lines  text))
        index-rule (map (fn [[^String rule-index rule-text]]
                          (vector (Integer/valueOf rule-index)
                                  (read-rule rule-text))) fields)]
    (into {} index-rule)))

(defn- read-problem
  ([] (read-problem (io/resource "day19-input.txt")))
  ([location]
   (let [[rules-text messages-text] (string/split (slurp location) #"\n\n")]
     {:rules (read-rules rules-text)
      :messages (string/split-lines messages-text)})))

(defn- modify-rules [rules]
  (merge rules {8 #{[42] [42 8]}
                11 #{[42 31] [42 11 31]}}))

(defn- explicit-rule? [rule]
  (not (some number? (flatten (seq rule)))))

(defn replace-in-rule [rule-list index value]
  (let [mangled-rule (map (fn [rule-item]
                            (cond
                              (and (number? rule-item)
                                   (= rule-item index)) value
                              (vector? rule-item) (replace-in-rule rule-item index value)
                              :default rule-item))
                          rule-list)]
    (into (empty rule-list) mangled-rule)))

(defn- replace-rule [rules [rule-index rule-value]]
  (into {}
        (map (fn [[index rule]]               
               (vector index (if (string? rule)
                               rule
                               (replace-in-rule rule rule-index rule-value))))
             (dissoc rules rule-index))))

(defn- get-first-explicit-rule [rules exclude-rules]
  (first
   (filter (fn [[_ rule]]
             (explicit-rule? rule))
           (filter (fn [[index _]]
                     (not (exclude-rules index)))
                   rules))))

(defn- rules-to-combinations
  ([rules]
   (rules-to-combinations rules #{}))
  ([rules exclude-rules]
   (if (= (count rules) 1)
     (get rules 0)
     (let [rule (get-first-explicit-rule rules exclude-rules)]
       (if rule
         (recur (replace-rule rules rule) exclude-rules)
         rules)))))

(defn- combinations-to-regexp [combinations]
  (re-pattern
   (walk/postwalk (fn [form]
                    (cond
                      (and (vector? form)
                           (every? string? form)) (apply str form)
                      (and (set? form)
                           (every? string? form)) (format "(%s)" (apply str (interpose "|" form)))
                      :default form
                      ))
                  combinations)))

(defn- get-a-minimum-expression [combinations]
  (walk/postwalk (fn [x]
                   (cond 
                     (vector? x) (apply str x) 
                     (set? x) (last (sort-by count x))
                     :default x)) 
                 combinations))

(defn- get-regexp-data-for-rule
  "Returns the regexps and minimum expression and sizes matched by the specified rule.

  The procedure works only for 42 and 31, due to the particular way
  the problem is stated."
  [combinations rule-index]
  (let [combinations-spec-rule (get combinations rule-index)
        minimum-generated-for-rule (get-a-minimum-expression combinations-spec-rule)]
    {:regexp (combinations-to-regexp combinations-spec-rule)
     :minimum-sample minimum-generated-for-rule
     :minimum-length (count minimum-generated-for-rule)}))

(defn- get-regexp-data-31-and-42
  "Returns the regexps and minimum expression and sizes matched by rules 42 and 31

  The numbers are hard-coded as part of the problem, as 42 and 31 are rules precursors
  to rule 8 and 11 (the only looping rules and - fortunately - the only children of rule 0)."
  [rules]
  (let [updated-rules (modify-rules rules)
        combinations (rules-to-combinations updated-rules #{8 11 0 42 31})]
    {31 (get-regexp-data-for-rule combinations 31)
     42 (get-regexp-data-for-rule combinations 42)}))

;;; TODO/FIXME better or at least decent estimate
(defn- get-plausible-combinations
  "Return a range  of [N M] pairs that could match the string.

  [N,M] are such that:

  repeat(regexp42, N + M) + repeat(regexp31, M)

  are around the size of S (equal or larger).

  It's 2AM and I'm tired, so it's a very very generous
  ballpark estimate, and nothing else.

  The net result is that I will do a lot more iterations, but
  it's not a problem performances-wise.

  PS: for the records, I *tried* to use algebra and failed
  miserably. As I said: 2AM"
  [string S31 S42]
  (let [slen (count string)
        ratio-31 (int (Math/ceil (/ slen S31)))
        ratio-42 (int (Math/ceil (/ slen S42)))]
    (filter (fn [[M N]]
              (<= (+ (* S31 M) (* S42 (+ M N))) slen))
            (for [M (range 1 (inc ratio-31))
                  N (range 1 (inc ratio-42))]
              (vector M N)))))

(defn- combine
  "Returns a (N+M)R42(M)R31 combination of the 2 regexps"
  [M N R31 R42]
  {:pre [(integer? N) (integer? M)]}
  (->> (concat (repeat (+ N M) R42)
              (repeat M R31))
       (apply str)
       re-pattern))

(defn- string-matches-combination?
  "Returns a non falsy value if string matches (N+M)R42(M)R31"
  [string [M N] R31 R42]
  {:pre [(string? string) (integer? N)]}
  (let [resulting-regexp (combine M N R31 R42)]
    (re-matches resulting-regexp string)))

(defn- string-matches-a-combination-range?
  "Return the string if it matches any of the M×N combinations"
  [string MN-list R31 R42]
  (some #(string-matches-combination? string % R31 R42) MN-list))

(defn- get-31-42-matching-messages
  "Returns the list of strings matching the looping rules"
  [rules messages]
  {:pre [(= [8 11] (get rules 0))]}
  (let [regexp-data (get-regexp-data-31-and-42 rules )
        {regexp-31 :regexp, S31 :minimum-length} (get regexp-data 31)
        {regexp-42 :regexp, S42 :minimum-length} (get regexp-data 42)]    
    (filter (fn [[message MN-list]]              
              (string-matches-a-combination-range? message MN-list regexp-31 regexp-42 ))
         (map #(vector % (get-plausible-combinations % S31 S42)) messages))))

(defn- part-2
  "Solve the second part of the exercise

  The solution assumes that rule 0 is [8 11], which from the problem description seems to be the case"
  [{rules :rules, messages :messages}]
  (assert (= [8 11] (get rules 0)) "This solution works only if rule 0 is [8 11]")
  (count (get-31-42-matching-messages rules messages)))

(defn- part-1 [{rules :rules, messages :messages}]
  (let [regexp (combinations-to-regexp (rules-to-combinations rules))]
    (count (filter #(re-matches regexp %) messages))))

(defn day19
  "Print the solution for day 19.

  I feel like I have kind of \"cheated\" by piggybacking regular expressions
  but - on the other end - knowledge is power…"
  []
  (println "*** Results for day19:")
  (let [code (read-problem)]
    (println "Number of messages matching rule 0:" (part-1 code))
    (println "Number of messages matching modified rules:"(part-2 code))))
