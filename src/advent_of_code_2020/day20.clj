(ns advent-of-code-2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

;;; TODO/FIXME this could benefit from a lot of optimization (most of the time
;;; is spent finding the jigsaw matches)
;;; a) cache some operations (many of them can be cached!). Memoize for the win!
;;; b) refine some checks: why do 20 transform for a tile that has not the required side?

(defn- read-id [^String text]
  (Integer/valueOf ^String (second (re-matches #"Tile ([0-9]+):" text))))

(defn- read-tile [text]
  (let [[title & tile-lines] (string/split-lines text)]
    {:id (read-id title)
     :data (vec tile-lines)}))

(defn- read-problem
  ([] (read-problem (io/resource "day20-input.txt")))
  ([location]
   (let [tiles (string/split (slurp location) #"\n\n")]
     (into #{}
           (map read-tile tiles)))))

(defn- get-sides
  "Dictionary of sides in the current state"
  [tile-data]
  {:top (first tile-data)
   :right (apply str (map last tile-data))
   :bottom (apply str (reverse (last tile-data)))
   :left (apply str (reverse (map first tile-data)))})

(defn- rotate
  "Turn the data 90° clockwise"
  ([tile-data]
   (rotate tile-data 1))
  ([tile-data n]
   (if (zero? n)
     tile-data
     (let [new-data (apply mapv (fn [& column] (apply str (reverse column))) tile-data)]
       (recur new-data (dec n))))))

(defn- flip
  "Flip the data sideways"
  [tile-data]
  (mapv (comp #(apply str %) reverse) tile-data))

(defn- flop
  "Flip the data upwards"
  [tile-data]
  (vec (reverse tile-data)))

(defn- get-unordered-sides
  "Get a list of mirror-independent sides for the current tile.

  Those sides can be safely compared with other tiles with uniqueness"
  [tile-data]
  {:pre [(nil? (:id tile-data))]}
  (let [plain-sides (get-sides tile-data)
        flip-side #(apply str (reverse %))]
    (vector #{(:top plain-sides)
              (flip-side (:top plain-sides))}
            #{(:left plain-sides)
              (flip-side (:left plain-sides))}
            #{(:bottom plain-sides)
              (flip-side (:bottom plain-sides))}
            #{(:right plain-sides)
              (flip-side (:right plain-sides))})))

(defn- sides-tile-pairs
  [tile]
  {:pre [(:id tile)]}
  (let [id (:id tile)]
    (map (fn [side]
           [side id])
         (get-unordered-sides (:data tile)))))

(defn- catalogue-sides
  "Returns a map of unordered sides vs tiles ID owning them"
  [tiles]
  (reduce (fn [map [side id]]
            (update map side
                    (fn [prev]
                      (if prev
                        (conj prev id)
                        (hash-set id)))))
          {}
          (apply concat (map sides-tile-pairs tiles))))

(defn- get-sides-owners
  "Return a list of IDs with all owners of the sides"
  [catalogue sides]
  {:pre [(not (map? sides))]}
  (apply set/union (map #(get catalogue %) sides)))

(defn- get-neighbors [catalogue tile]
  (let [unordered-sides (get-unordered-sides (:data tile))
        sides-owners (get-sides-owners catalogue unordered-sides)]
    (disj sides-owners (:id tile))))

(defn- corner-tile? [catalogue tile]
  (= (count (get-neighbors catalogue tile)) 2))

(defn- part-1 [tiles]
  (let [catalogue (catalogue-sides tiles)]
    (apply * (map :id (filter (partial corner-tile? catalogue) tiles)))))

(def ^:private sea-monster ["                  # "
                            "#    ##    ##    ###"
                            " #  #  #  #  #  #   "])

(def ^:private all-transforms (for [flip-f [identity flip]
                                    flop-f [identity flop]
                                    rotate-f [#(rotate % 0)
                                              #(rotate % 1)
                                              #(rotate % 2)
                                              #(rotate % 3)]]
                                (list flip-f flop-f rotate-f)))

(defn- get-all-transforms
  "lazy-sequence of all mirror/rotations of the tile data (with some extra…)"
  [tile-data]
  {:pre [(not (:id tile-data))]}
  (map (fn [[op1 op2 op3]]
         (-> tile-data op1 op2 op3)) all-transforms))

(defn- get-empty-board-state [tiles]
  (let [side (int (Math/sqrt (count tiles)))]
    {:board (vec (repeat side (vec (repeat side nil))))
     :side side 
     :catalogue (catalogue-sides tiles)
     :unused (into {} (map #(vector (:id %) %) tiles))}))

(defn- border-side? [catalogue tile-side]
  (= 1 (count (second (first (filter (fn [[side-set tiles]]
                                       (contains? side-set tile-side))
                                     catalogue))))))

(defn- wanted-side? [catalogue side-keyword all-sides constraints]
  (let [constraint-side (get constraints side-keyword)
        tile-side (get all-sides side-keyword)]
   (if (= :border constraint-side)
     (border-side? catalogue tile-side)
     (= tile-side constraint-side))))

(defn- satisfies-constraints? [catalogue tile-data constraints]
  (let [sides (get-sides tile-data)]
    (and
     (wanted-side? catalogue :left sides constraints)
     (wanted-side? catalogue :top sides constraints))))

(defn- get-any-transform-matching-constraints
  "Can be optimized, e.g. to discard tiles that don't just have the required side"
  [catalogue tile-data constraints]
  (first
   (filter #(satisfies-constraints? catalogue % constraints)
           (get-all-transforms tile-data))))

(defn- find-first-tile-for-constraints
  "Returns the [id oriented-tile-data] of the first unused tile matching the specified constraints"
  [catalogue unused-tiles constraint]
  {:pre [(map? constraint)
         (map? unused-tiles)
         (not (empty? unused-tiles))]}
  (some #(and (second %) %)
        (map (fn [[id tile]]
               (vector id
                       (get-any-transform-matching-constraints catalogue
                                                               (:data tile)
                                                               constraint)))
             unused-tiles)))

(defn- get-left-constraint [board row column]
  (if (= column 0)
    :border
    (let [left-cell (get-in board [row (dec column)])]
      (apply str (reverse (:right (get-sides (:data left-cell))))))))

(defn- get-top-constraint [board row column]
  (if (= row 0)
    :border
    (let [top-cell (get-in board [(dec row) column])]
      (apply str (reverse (:bottom (get-sides (:data top-cell))))))))

(defn- get-constraints-for-cell
  "Return a map with :left :top which can be either an actual border or :border"
  [board-state row column]
  {:left (get-left-constraint (:board board-state) row column)
   :top (get-top-constraint (:board board-state) row column)})

(defn- update-board [board-state row column]
  (let [constraints (get-constraints-for-cell board-state row column)
        [id tile-data] (find-first-tile-for-constraints (:catalogue board-state)
                                                        (:unused board-state)
                                                        constraints)]
    (assert id (format "Constraints not satisfied for %d,%d?" row column))
    (merge board-state
           {:board (assoc-in (:board board-state)
                             [row column]
                             {:id id, :data tile-data})
            :unused (dissoc (:unused board-state) id)})))

(defn- is-empty-board-state? [board-state]
  (let [side (:side board-state)]
   (= (* side side)
      (count (:unused board-state)))))

(defn- fill-board [empty-board-state]
  {:pre [(is-empty-board-state? empty-board-state)]}
  (let [side (:side empty-board-state)
        ordered-indices (for [row (range side), column (range side)] (vector row column))]
    (reduce #(apply update-board % %2) empty-board-state ordered-indices)))

(defn- create-filled-board-state [tiles]
  (fill-board (get-empty-board-state tiles)))

(defn- remove-tile-data-border [tile-data]
  (let [columns (count (first tile-data))
        expected-size (- columns 2)]
    (mapv #(apply str (take expected-size (rest %))) (take expected-size (rest tile-data)))))

(defn- concatenate-row
  "Concatenate a row of (possibly reduced) tiles"
  [tile-data-list]
  (apply mapv (fn [& lines]
                (apply str lines))
         tile-data-list))

(defn- concatenate-tiles-data
  "Remove the borders and concatenate the tiles in a single rectangular tile"
  [board-data]
  (let [reduce-row #(map remove-tile-data-border (map :data %))]
    (mapv #(-> % reduce-row concatenate-row) board-data)))

(defn- filled-board-to-tile-data
  "Convert the board state to a big tile-data-like vector"
  [board-state]
  (vec (apply  concat (concatenate-tiles-data (:board board-state)))))

(defn- count-signals [rows]
  (apply + (map #(count (filter #{\#} %)) rows)))

(def ^:private sea-monster-signature (count-signals sea-monster))

(defn- subtile
  [tile-data row column height width]
  {:post [(= height (count %)) (= width (count (first %)))]}
  (let [wanted-rows (take height (drop row tile-data))]
    (mapv #(.substring ^String % column (+ column width)) wanted-rows)))

;; TODO/FIXME I'm not optimizing to early discard non-monster tiles…
(defn- possible-monster-piece?
  "1 is \"could be a monster piece\", 0 is \"could be anything\" "
  [map-char monster-char]
  (if (and (= monster-char \#)
        (= map-char \#)) 1 0))

(defn- sum-monster-line [line monster-line]
  (apply + (map possible-monster-piece? line monster-line)))

(defn- contains-monster? [tile]
  (= sea-monster-signature
     (apply + (map sum-monster-line tile sea-monster))))

(defn- pseudo-convolution
  "Perform a 2D-convolution-like operation on the map.

  Returns the matches found.

  I don't need to flip the sample because I will test all orientations
  anyway."
  [map]
  {:pre [(vec map) (vec sea-monster)]}
  (let [monster-rows (count sea-monster)
        monster-columns (count (first sea-monster))
        map-rows (count map)
        map-columns (count (first map))
        steps (for [row (range (- map-rows monster-rows))
                    column (range (- map-columns monster-columns))]
                (subtile map row column monster-rows monster-columns))]
    (filter contains-monster? steps)))

(defn- get-pseudo-convolutions
  "Find the first transformation/mirror of the map that has convolutions, and returns them"
  [tile-data]
  (let [transforms (get-all-transforms tile-data)]
    (first (filter (complement empty? ) (map #(pseudo-convolution %) transforms)))))

(defn- part-2 [tiles]
  (let [filled-board (create-filled-board-state tiles)
        tile-map (filled-board-to-tile-data filled-board)
        convolutions (count (get-pseudo-convolutions tile-map))]
    (- (count-signals tile-map) (* convolutions sea-monster-signature))))

(defn day20
  "Print the solution for day 20

  Though to get right: a lot of details that could go wrong (and did…).
  
  Also, a horribly convoluted protocol-based attempt has been mercifully
  obliterated by the magic of git."
  []
  (println "*** Results for day20:")
  (let [problem-input (read-problem)]
    (println "Product of corner tiles IDs:" (part-1 problem-input))
    (println "Habitat's water roughness:" (part-2 problem-input))))
