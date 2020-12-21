(ns advent-of-code-2020.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- read-product-description
  "Assume every line has a '(contains …)' part, assumes each allergen is a single word (true for my input)"
  [text]
  (let [[_ ingredients allergens] (re-matches #"(.+) \(contains (.+)\)" text)]
    {:ingredients (into #{} (string/split ingredients #" "))
     :allergens (into #{} (map keyword (string/split allergens #", ")))}))

(defn- read-problem
  ([] (read-problem (io/resource "day21-input.txt")))
  ([location]
   (map read-product-description (string/split-lines (slurp location)))))

(defn- all-contain-ingredient? [ingredients-list ingredient]
  (every? #(contains? % ingredient) ingredients-list))

(defn- get-ingredients-with-allergen [descriptions allergen]
  (map :ingredients
       (filter (fn [{ingredients :ingredients, allergens :allergens}]
             (contains? allergens allergen))
           descriptions)))

(defn- can-product-be-allergen? [descriptions ingredient allergen]
  (let [ingredients-list (get-ingredients-with-allergen descriptions allergen)]
    (all-contain-ingredient? ingredients-list ingredient)))

(defn- all-allergens [descriptions]
  (reduce set/union (map :allergens descriptions)))

(defn- can-ingredient-be-allergen? [descriptions ingredient]
  (some #(can-product-be-allergen? descriptions ingredient %)
        (all-allergens descriptions)))

(defn- all-ingredients [descriptions]
  (reduce set/union (map :ingredients descriptions)))

(defn- compute-non-allergenic [descriptions]
  (let [is-not-allergen? (complement #(can-ingredient-be-allergen? descriptions %))]
    (into #{}
          (filter is-not-allergen? (all-ingredients descriptions)))))

(defn- count-non-allergens-occurrences [descriptions]
  (let [non-allergenic (compute-non-allergenic descriptions)]
    (apply + (map count
                  (map #(set/intersection % non-allergenic)
                       (map :ingredients descriptions))))))

(defn- part-1 [products]
  (count-non-allergens-occurrences products))

(defn- remove-ingredients-from-list [ingredients to-remove]
  {:pre [(set? ingredients) (set? to-remove)]}
  (set/difference ingredients to-remove))

(defn- reduce-description
  "Remove ingredients from the products from the description"
  [descriptions ingredients-to-remove]
  (map (fn [description]
         (update description :ingredients #(remove-ingredients-from-list % %2) ingredients-to-remove))
       descriptions))

(defn- remove-ingredients-from-rules  
  [rules ingredients-to-remove]
  {:pre [(map? rules) (set? ingredients-to-remove)]}
  (into {}
        (map (fn [[allergen ingredients]]
               (vector allergen (set/difference ingredients ingredients-to-remove)))
             rules)))

(defn- get-univocal-rules [rules]
  (into {}
        (map (fn [[allergen ingredients]]
               [allergen (first ingredients)])
             (filter (fn [[_ ingredients]]
                       (= 1 (count ingredients)))
                     rules))))

 (defn- remove-allergens-from-rules
   [rules allergens]
   {:pre [(map? rules) (seq? allergens)]}
   (apply dissoc rules allergens))

(defn- reduce-possibilities 
  "Try to find and reduce unique associations"
  [{rules :rules, associations :known-associations}]
  (let [univocal-rules (get-univocal-rules rules)
        solved-allergens (map first univocal-rules)
        solved-ingredients (into #{} (map second univocal-rules))]
    {:rules (-> rules
                (remove-allergens-from-rules solved-allergens)
                (remove-ingredients-from-rules solved-ingredients))
     :known-associations (merge associations univocal-rules)}))

(defn- reduce-all-possibilities [knowledge]
  (if (empty? (:rules knowledge))
    knowledge
    (let [after-reduction (reduce-possibilities knowledge)]
      (if (= after-reduction knowledge)
        after-reduction
        (recur after-reduction)))))

(defn- count-non-allergens-in-product [ingredients allergens]
  (count (filter (complement allergens) ingredients)))

(defn- count-non-allergens [ingredients-list allergens]
  {:pre [(set? allergens)]}
  (let [count-function #(count-non-allergens-in-product % allergens)]
    (apply + (map count-function ingredients-list))))

(defn- has-allergen? [allergen description]
  (-> description
      :allergens
      (contains? allergen)))

(defn- products-with-allergen [allergen descriptions]
  (filter (partial has-allergen? allergen) descriptions))

(defn- products-by-allergen [descriptions]
  (let [allergens (all-allergens descriptions)]
    (into {} (map #(vector % (map :ingredients (products-with-allergen % descriptions)))
                  allergens))))

(defn- potential-allergens-mapping [description]
  (let [allergen-products (products-by-allergen description)]
    (into {}
          (map (fn [[allergen products]]
                 (vector allergen (reduce set/intersection products)))
               allergen-products))))

(defn- create-initial-knowledge [description]
  {:rules (potential-allergens-mapping description)
   :known-associations {}})

(defn- solve-associations [descriptions]
  (let [reduced-descriptions (reduce-description descriptions (compute-non-allergenic descriptions))
        initial-knowledge (create-initial-knowledge reduced-descriptions)
        {:keys [rules known-associations]} (reduce-all-possibilities initial-knowledge)]
    (assert (empty? rules) "The problem cannot be solved by direct induction (mine was…)")
    known-associations))

(defn- pretty-print-associations [allergen-to-product]
  (apply str (interpose "," (map second (sort-by first allergen-to-product)))))

(defn- part-2 [descriptions]
  (pretty-print-associations
   (solve-associations descriptions)))

(defn day21
  "Print the solution for day 21"
  []
  (println "*** Results for day21:")
  (let [descriptions (read-problem)]
    (println "Non-allergenic ingredients repetitions:" (part-1 descriptions))
    (println "Allergenic ingredients:" (part-2 descriptions))))
