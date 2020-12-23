(ns advent-of-code-2020.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def current-game (atom 1))
(def current-round (atom 1))

(defn my-print [message & args]
 (comment  (println (apply format message args))))

(defn- read-deck [text]
  (let [values (drop 1 (string/split-lines text))]
    (mapv #(Integer/valueOf ^String %) values)))

(defn- read-problem
  ([] (read-problem (io/resource "day22-input.txt")))
  ([location]
   (let [raw-decks (string/split (slurp location) #"\n\n")]
     (mapv read-deck raw-decks))))

(defn- append-cards
  [deck card1 card2]
  {:pre [(> card1 card2)
         (vector? deck)]
   :post [(vector? %) (= (count %) (+ (count deck) 2))]}
  (vec (concat deck (list card1 card2))))

(defn- extract-card [deck]
  (let [[card & remaining] deck]
    [card (vec remaining)]))

(defn- compute-round-winner [decks]
  {:pre [(vector? decks)
         (= 2 (count decks))
         (not (empty? (first decks)))
         (not (empty? (second decks)))]}
    (let [[card1 deck1] (extract-card (first decks))
          [card2 deck2] (extract-card (second decks))]
    (cond
      (= card1 card2) (throw (IllegalStateException. "Impossible condition: same cards!"))
      (> card1 card2) {:winner :player1, :new-decks [(append-cards deck1 card1 card2) deck2]}
      (> card2 card1) {:winner :player2, :new-decks [deck1 (append-cards deck2 card2 card1)]})))

(defn- do-round
  "evolve the decks 1 turn, returns the new decks"
  [decks]
  (:new-decks (compute-round-winner decks)))

(defn- calculate-score [deck]
  (apply + (map #(apply * %) (map-indexed #(vector (inc %) %2) (reverse deck)))))

(defn- do-match [decks]
  (let [new-state (do-round decks)]
    (if (some empty? new-state)
      (apply max (map calculate-score new-state))
      (recur new-state))))

(defn- part-1 [decks]
  (do-match decks))

(def ^:private resolve-game)


(defn- assert-ok-deck [deck]
  (and (vector? deck)
       (empty? (filter (complement integer?) deck))))

(defn- assert-ok-decks [decks]
  (and (assert-ok-deck (first decks))
       (assert-ok-deck (second decks))))

(defn- assert-ok-state [state]
  (and (assert-ok-decks (:decks state))
       (set? (:visited state))))

(defn- increase-deck [deck first-card second-card]
  {:pre [(assert-ok-deck deck) (number? first-card) (number? second-card)]
   :post [(assert-ok-deck %)]}
  (conj (conj deck first-card) second-card))

(defn- recombine-decks [winner [card1 card2] [deck1 deck2]]
  {:post [(assert-ok-decks %)]}
  (let []
    (vector
     (if (#{:player1 :recursive} winner) (increase-deck deck1 card1 card2) deck1)
     (if (= winner :player2) (increase-deck deck2 card2 card1) deck2))))


(defn- resolve-by-subgame [{:keys [visited decks] :as state}]
  {:pre [(assert-ok-decks decks)]
   :post [(assert-ok-state (:state %))]}
  (my-print "Resolving %d-%d by sub-game" @current-game @current-round)
  (swap! current-game inc)
  (let [round @current-round]
    (reset! current-round 1)
   (let [[card1 remaining1] (extract-card (first decks))
         [card2 remaining2] (extract-card (second decks))
         ;; It's very important that you take only the appropriate number of cards, otherwise you may end up
         ;; at 4AM trying to solve with caching, dynamic programming and groggy math what turns out
         ;; to be an infinite sequence. 
         sub-game-decks [(vec (take card1 remaining1))
                         (vec (take card2 remaining2))]
         sub-game-winner  (:winner (resolve-game {:visited #{}, :decks sub-game-decks}))
         new-decks (recombine-decks sub-game-winner
                                    [card1 card2]
                                    [remaining1 remaining2])]
     (swap! current-game dec)
     (reset! current-round round)
     {:winner sub-game-winner
      :state {:decks new-decks
              :visited (conj visited new-decks)}})))

(defn- resolve-game-step-by-higher-card [{:keys [visited decks] :as state}]
  {:pre [(assert-ok-state state)]
   :post [(assert-ok-decks (-> % :state :decks))
          (assert-ok-state (:state %))]}
  (my-print "Resolving %d-%d by higher card" @current-game @current-round)
  (let [{new-decks :new-decks, winner :winner} (compute-round-winner decks)
        new-visited (conj visited new-decks)]
    {:winner winner
     :state {:visited new-visited
             :decks new-decks}}))

(defn- card-trumps-deck?
  "true if the card is higher than the number of cards in the deck"
  [card deck]
  {:pre [(integer? card) (assert-ok-deck deck)]}
  (> card (count deck)))

(defn- higher-card-mode? [decks]
  {:pre [(assert-ok-decks decks)]}
  (let [[card1 remaining1] (extract-card (first decks))
        [card2 remaining2] (extract-card (second decks))]
    (or (card-trumps-deck? card1 remaining1)
        (card-trumps-deck? card2 remaining2))))

(defn- step-game [{:keys [visited decks] :as state}]
  {:pre [(assert-ok-decks decks)
         (set? (:visited state))
         (not (contains? visited decks))]}
  (my-print "Stepping game %d - %d" @current-game @current-round)
  (if (higher-card-mode? decks)
    (resolve-game-step-by-higher-card state)
    (resolve-by-subgame state)))

(defn- resolve-game
  [state]
  (if (contains? (:visited state) (:decks state))
    (do
      (my-print "Solving by recursion Game %d-%d!" @current-game @current-round)
      {:winner :player1, :state state})
    (let [new-visited (conj (:visited state) (:decks state))
          {result :winner, new-state :state :as stepped-result} (step-game state)]
      (cond
        (empty? (-> new-state :decks first)) {:winner :player2, :state new-state}
        (empty? (-> new-state :decks second)) {:winner :player1, :state new-state}
        :default (do
                   (swap! current-round inc)
                   (recur {:decks (:decks new-state) :visited new-visited}))))))

(defn- create-initial-recursive-state [decks]
  {:visited #{}
   :decks decks})

(defn- play-recursive-game [decks]
  (reset! current-game 1)
  (reset! current-round 1)
  (let [resolved-game (resolve-game (create-initial-recursive-state decks))]
   (apply max (map calculate-score (-> resolved-game :state :decks)))))

(defn- part-2 [decks]
  (play-recursive-game decks))

(defn day22
  "Print the solution for day 22

  It turns out that it's easier when you read all instructions :("
  []
  (println "*** Results for day22:")
  (let [decks (read-problem)]
    (println "Winning player's score (normal combat):" (part-1 decks))
    (println "Winning player's score (recursive comat):" (part-2 decks))))
