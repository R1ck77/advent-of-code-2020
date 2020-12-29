(ns advent-of-code-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; Fields:
;; byr (Birth Year)
;; iyr (Issue Year)
;; eyr (Expiration Year)
;; hgt (Height)
;; hcl (Hair Color)
;; ecl (Eye Color)
;; pid (Passport ID)
;; cid (Country ID)

(defn in-range? [[min max] s-value]
  (try
    (let [number (Integer/valueOf s-value)]
      (and (>= number min) (<= number max)))
    (catch NumberFormatException e nil)))

(defn- parse-length [raw-value]
  (let  [[_ number-s unit] (re-matches #"([0-9]+)(in|cm)" raw-value)]
    [number-s unit]))

(defn length-range? [metric-range imperial-range raw-value]
  (let [[s-value unit] (parse-length raw-value)]
    (in-range? (if (= "in" unit)
                 imperial-range
                 metric-range)
               s-value)))

(defn valid-color? [value]
  (re-matches #"#[0-9a-f]{6}" value))

(defn serial-number? [digits value]
  (re-matches #"[0-9]{9}" value))

(def mandatory-fields {:byr (partial in-range? [1920 2002])
                       :iyr (partial in-range? [2010 2020])
                       :eyr (partial in-range? [2020 2030])
                       :hgt (partial length-range? [150 193] [59 76])
                       :hcl valid-color?
                       :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                       :pid (partial serial-number? 9)})

(def optional-fields {:cid identity})

(defn- read-group [text]
  (apply str (interpose " " (string/split-lines text))))

(defn- read-problem
  "Read the input as a list of raw passport definitions"
  ([] (read-problem (io/resource "day4-input.txt")))
  ([location]
   (map read-group
        (string/split
         (slurp location) #"\n\n"))))

(defn- parse-field [raw-field]
  (let [[key remaining] (split-with #(not= % \:) raw-field)]
    [(keyword (apply str key)) (apply str (rest remaining))]))

(defn- verify-definition
  "check that the passport only has the permitted fields"
  [passport-map]
  (if (some #(not (or (mandatory-fields %)
                       (optional-fields %))) (map first passport-map))
    (throw (RuntimeException. (str "Invalid passport found: " passport-map)))
    passport-map))

(defn- parse-definition [raw-definition]
  (verify-definition (into {} (map parse-field (string/split raw-definition #"[ ]")))))

(defn- valid-fields? [passport-map]
  (let [actual-keys (apply hash-set (keys passport-map))]
    (every? actual-keys (map first mandatory-fields))))

(defn- valid-values? [passport-map]
  (and (valid-fields? passport-map)
       (every? (fn [[field value]]
                 (let [rule (or (mandatory-fields field)
                                (optional-fields field))]
                   (rule value)))
               passport-map)))

(defn- count-valid-passports [raw-definitions validation-predicate]
  (count (filter validation-predicate (map parse-definition raw-definitions))))

(defn day4
  "Print the solutions for day 4"
  []
  (println "*** Results for day4:")
  (let [data (read-problem)]
    (println "Valid number of passports (relaxed check): " (count-valid-passports data valid-fields?))
    (println "Valid number of passports (thorough check): " (count-valid-passports data valid-values?))))
