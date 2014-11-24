(ns clj-markov.generation
  (:require [clj-markov.training :refer [shift-and-append]]))

(defn generate*
  [{:keys [state output]} chain]
  (let [antecedents (get chain state)
        weight-sum (reduce + (vals antecedents))
        selection (rand weight-sum)
        [_ next-token] (reduce (fn [[position selected-token] [token weight]]
                                 (if selected-token
                                   [-1 selected-token]
                                   (let [position' (+ position weight)]
                                     (if (> position' selection)
                                       [-1 token]
                                       [position' nil]))))
                               [0 nil]
                               (seq antecedents))]
    {:state (shift-and-append state next-token)
     :output (conj output (name next-token))}))

(defn generate
  ([chain amount]
   (let [s0 (rand-nth (keys (dissoc chain :clj-markov.training/state)))]
     (generate chain s0 amount)))
  ([chain starting-state amount]
   (:output (reduce (fn [sm _] (generate* sm chain))
                    {:state starting-state, :output []}
                    (range amount)))))
