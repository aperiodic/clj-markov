(ns clj-markov.generation
  (:require [clj-markov.training :refer [shift-and-append]]))

(defn generate
  [chain amount]
  (loop [state (rand-nth (keys chain)), out []]
    (if (= (count out) amount)
      out
      (let [antecedents (get chain state)
            weight-sum (reduce + (vals antecedents))
            selection (rand weight-sum)
            [_ next-token] (reduce (fn [[position selected-token] [token occurrences]]
                                     (if selected-token
                                       [-1 selected-token]
                                       (let [position' (+ position occurrences)]
                                         (if (> position' selection)
                                           [-1 token]
                                           [position' nil]))))
                                   [0 nil]
                                   (seq antecedents))]
        (recur (shift-and-append state next-token), (conj out (name next-token)))))))
