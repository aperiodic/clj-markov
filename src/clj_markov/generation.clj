(ns clj-markov.generation
  (:require [clj-markov.training :refer [shift-and-append]]))

(defn generate
  [chain amount]
  (loop [state (rand-nth (keys (dissoc chain :clj-markov.training/state))), out []]
    (if (= (count out) amount)
      out
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
        (recur (shift-and-append state next-token), (conj out (name next-token)))))))
