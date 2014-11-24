(ns clj-markov.training)

(defn shift-and-append
  [state token]
  (case (count state)
    1 [token]
    2 [(nth state 1) token]
    3 [(nth state 1) (nth state 2) token]
    4 [(nth state 1) (nth state 2) (nth state 3) token]
    5 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) token]
    6 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) (nth state 5) token]
    7 [(nth state 1) (nth state 2) (nth state 3) (nth state 4) (nth state 5) (nth state 6) token]
    (throw (IllegalArgumentException. "unsupported state length"))))

(defn- train*
  [chain token weight]
  (let [token-kw (keyword token)
        state (::state chain)
        state' (shift-and-append state token-kw)]
    (-> chain
      (update-in [state token-kw] (fnil + 0) weight)
      (assoc ::state state'))))

(def default-chain-length 2)

(defn new-chain
  ([] (new-chain default-chain-length))
  ([length]
   {::state (-> (repeat length nil) vec)}))

(defn reset-chain
  [chain]
  (let [length (count (::state chain))]
    (if (zero? length)
      chain
      (assoc chain ::state (repeat length nil)))))

(def default-training-opts
  {:reset-state? true
   :weight 1})

(defn train
  ([tokens] (train (new-chain) tokens))
  ([chain tokens] (train chain tokens {}))
  ([chain tokens opts]
   (let [{:keys [reset-state? weight]} (merge default-training-opts opts)
         chain' (if reset-state? (reset-chain chain) chain)]
     (reduce #(train* %1 %2 weight) chain' tokens))))

(defn train-on-corpuses
  ([corpuses] (train-on-corpuses corpuses default-chain-length))
  ([corpuses length]
   (let [sizes (map count corpuses)
         total-size (reduce + sizes)
         weights (map #(/ (float total-size) %) sizes)]
     (clojure.pprint/pprint weights)
     (reduce (fn [chain [corpus weight]] (train chain corpus {:weight weight}))
             (new-chain length)
             (map vector corpuses weights)))))
