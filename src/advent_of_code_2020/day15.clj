(ns advent-of-code-2020.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def ^:private input [0 1 4 13 15 12 16])

;;; Cheating on the initial side, as I know the sequence is 30000000
(def ^:dynamic *initial-array-size* 30000000)
(def ^:dynamic *array-increment* 2000000)

(defprotocol PositionsArchive
  (getLastPositions [this key])
  (updateArchive [this key position]))

;;; First version of the positions archive: it's good enough for
;;; both part 1 and 2 but still quite slow
;;; (it takes minutes on my Ryzen for the latter)
(defn- update-archive-map [archive-map next position]
  (update archive-map next (fn [old-value]
                       ;; slight optimization
                       (if old-value
                         (list position (first old-value))
                         (list position)))))

(defn- create-archive [archive-map]
  (reify PositionsArchive
    (getLastPositions [this key]
      (get archive-map key))
    (updateArchive [this key position]
      (create-archive (update-archive-map archive-map key position)))))

(defn- map-data-from-sequence [sequence]  
  (reduce (fn [dict [key position]]
            (update-archive-map dict key position))
          {}
          (map-indexed (fn [index value]
                         (vector value index))
                       sequence)))

(defn- map-based-archive-from-sequence [sequence]
  (create-archive (map-data-from-sequence sequence)))

;;; Second version of the positions archive, array based this time.
;;; It's as fast as (I think) it could be, but consumes a *lot* of
;;; memory.
;;; Note that without resolving the reflection, this performed *terribly*.
(defn- update-array-data! [array-data key position]
  (let [old-value (aget ^"[Ljava.lang.Object;" array-data (int key))]
    (aset ^"[Ljava.lang.Object;" array-data
          (int key)
          (if old-value
            (list position (first old-value))
            (list position)))))

(defn- resize-archive-array [archive-array index]
  (let [old-size (count archive-array)
        new-size (max (+ *array-increment* old-size)
                      (inc index))
        new-array (make-array Object new-size)]
    (println (format "Resizing from %d to %d…" old-size new-size))
    (System/arraycopy archive-array 0 new-array 0 old-size)
    new-array))

(defn- create-array-based-archive! [archive-array]
  (let [archive (atom archive-array)
        archive-size (count archive-array)]
    (reify PositionsArchive
      (getLastPositions [this key]
        (if (>= key archive-size)
          (reset! archive (resize-archive-array archive-array key)))
        (aget ^"[Ljava.lang.Object;" @archive (int key)))
      (updateArchive [this key position]
        (if (>= key archive-size)
          (reset! archive (resize-archive-array archive-array key)))
        (update-array-data! @archive key position)
        (create-array-based-archive! @archive)))))

(defn- array-data-from-sequence
  ([sequence max]
   (let [archive (make-array Object max)]
     (dorun
      (map-indexed (fn [position value]
                     (update-array-data! archive value position))
                   sequence))
     archive)))

(defn- array-based-archive-from-sequence
  ([sequence]
   (array-based-archive-from-sequence sequence *initial-array-size*))
  ([sequence initial-array-size]
   (create-array-based-archive! (array-data-from-sequence sequence initial-array-size))))

(defn- next-value
  "Compute the next value of the sequence"
  [last-value archive]
  (let [indices (.getLastPositions ^advent_of_code_2020.day15.PositionsArchive archive last-value)]
    (if (= (count indices) 1)
      0
      (apply - (take 2 indices)))))

(defn- generate-sequence
  ([sequence archive]
   (generate-sequence (first sequence) (count sequence) archive))
  ([last-value position archive]
   (let [next (next-value last-value archive)]
     (lazy-seq
      (cons next
            (generate-sequence next (inc position) (.updateArchive ^advent_of_code_2020.day15.PositionsArchive archive next position)))))))

(def ^:dynamic *archive-constructor* map-based-archive-from-sequence)

(defn- van-eck-sequence
  "The propert Van Eck's sequence starts with a seed of [0]

  The name and identification is courtesy of the OEIS foundation:

  http://oeis.org/A181391"
  [seed]
  (concat (seq seed)
          (generate-sequence (reverse seed)
                             (*archive-constructor* seed))))

(defn- nth-van-eck-sequence-value [seed value]
  (first
   (drop (dec value)
         (van-eck-sequence seed))))

(defn- part-2
  "Damn elves…"
  [seed]
  (nth-van-eck-sequence-value seed 30000000))

(defn- part-1 [seed]
  (nth-van-eck-sequence-value seed 2020))

(defn day15
  "Print the solutions for day 15"
  []
  (println "*** Results for day15:")
  (println "the 2020th number is:" (part-1 input))
  (with-redefs [*archive-constructor* array-based-archive-from-sequence]
   (println "the 30000000th number (it may take minutes) is:" (part-2 input))))
