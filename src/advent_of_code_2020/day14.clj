(ns advent-of-code-2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- number-to-binary
  "Convert a number to a list of bits (less significant first)"
  [value]
  (reverse
   (loop [value value
          bits '()]
     (if (zero? value)
       bits
       (recur (long (/ value 2))
              (conj bits (mod value 2)))))))

(defn- number-to-36-bits [value]
  (let [binary-representation (number-to-binary value)
        padding (repeat (- 36 (count binary-representation)) 0)]
    (concat binary-representation padding)))

(defn- read-memory-operation [text]
  (let [[_ address value] (re-matches #"mem\[([0-9]+)\] = ([0-9]+)" text)]
    [:mem
     (number-to-36-bits (Integer/valueOf address))
     (number-to-36-bits (Integer/valueOf value))]))

(defn- read-mask-bits
  "Return the mask as a list of bits (less significant bit first)"
  [field]
  (reverse
          (map {\X nil
                \1 1
                \0 0}
               field)))

(defn- read-mask [text]
  [:mask (read-mask-bits (last (string/split text #" ")))])


(defn- bit-to-value [bit value]
  (if (zero? value)
    0
    (bit-shift-left value bit)))

(defn- bits-to-number [bits]
  (apply + (map-indexed bit-to-value bits)))

(defn- line-to-operation [text]
  (if (.startsWith text "mask")
    (read-mask text)
    (read-memory-operation text)))

(defn- read-program
  ([]
   (read-program (io/resource "day14-input.txt")))
  ([location]
   (mapv line-to-operation
         (string/split-lines (slurp location)))))

(defprotocol MemoryUnit
  (setMask [this mask] "Returns a new unit object with the new mask and same memory setup")
  (setMemory [this operation] "Returns a new unit object with same mask but different memory")
  (getMemorySum [this] "Returns the sum of memory"))

(defn- update-memory [memory mask [address value]]  
  (assoc memory address (map #(case %2
                                0 0
                                1 1
                                nil %1) value mask)))

(defn- create-memory-unit
  ([]
   (create-memory-unit {} nil))
  ([memory current-mask]
   (reify MemoryUnit
     (setMask [this mask]
       (create-memory-unit memory mask))
     (setMemory [this operation]
       (create-memory-unit (update-memory memory current-mask  operation)
                           current-mask))
     (getMemorySum [this]
       (reduce #(+ % (bits-to-number (second %2))) 0 memory)))))

(defn- first-floating-bit [masked-address]
  {:pre [(vector? masked-address)]}
  (let [floating-indexed-bits (filter (comp nil? second)
                                      (map-indexed #(list % %2)
                                                   masked-address))]
    (-> floating-indexed-bits first first)))

(defn- remove-floating-bit
  "Convert the masked address into 2 addresses with a floating bit removed"
  [masked-address]
  {:pre [(vector? masked-address)]}
  (let [index (first-floating-bit masked-address)]
    (list (assoc masked-address index 0)
          (assoc masked-address index 1))))

(defn- is-floating-address? [masked-address]
  {:pre [(vector? masked-address)]}
  (not
   (every? identity masked-address)))

(defn- generate-addresses
  "Take a list of addresses where only the first can have floating bits, and return an expanded list of non floating addresses.

  Could be made tail recursive using 2 data structures instead of a single list, but for 36 levels, why bother?"
  [current-list]
  (let [unresolved-address (first current-list)
        resolved-addresses (rest current-list)]
   (if (not (is-floating-address? unresolved-address))
     current-list
     (let [[first-unresolved second-unresolved] (remove-floating-bit unresolved-address)]
       (concat (generate-addresses (list first-unresolved))
               (generate-addresses (list second-unresolved))
               resolved-addresses)))))

(defn- mask-address
  "Convert the address to a (possibly) floating one, using the supplied mask"
  [mask address]
  {:post [(vector? %)]}
  (mapv #(case %
          0 %2
          1 1
          nil nil) mask address))

(defn- update-memory-decoder [memory mask [address value]]
  (let [masked-address (mask-address mask address)]
    (merge memory (into {} (map #(vector % value) (generate-addresses (list masked-address)))))))

(defn- create-decoder-unit
  ([]
   (create-decoder-unit {} nil))
  ([memory current-mask]
   (reify MemoryUnit
     (setMask [this mask]
       (create-decoder-unit memory mask))
     (setMemory [this operation]
       (create-decoder-unit (update-memory-decoder memory current-mask operation)
                            current-mask))
     (getMemorySum [this]
       (reduce #(+ % (bits-to-number (second %2))) 0 memory)))))

(defn- process-instruction [memory-unit instruction]
  (case (first instruction)
    :mask (.setMask memory-unit (second instruction))
    :mem (.setMemory memory-unit (rest instruction))))

(defn- compute-memory-sum [memory-unit program]
  (.getMemorySum
   (reduce process-instruction memory-unit program)))

(defn day14
  "Print the solutions for day 14"
  []
  (println "*** Results for day14:")
  (let [program (read-program)]
    (println "Sum of the memory (bitmask unit):" (compute-memory-sum (create-memory-unit)
                                                                                     program))
    (println "Sum of the memory (decoder unit):" (compute-memory-sum (create-decoder-unit)
                                                                                     program))))
