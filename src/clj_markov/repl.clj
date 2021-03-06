(ns clj-markov.repl
  (:require [clj-markov.generation :refer [generate generate-sentence]]
            [clj-markov.tokenization :refer [tokenize-file]]
            [clj-markov.training :refer [new-chain train train-on-corpuses]]))

(defn concat-token
  "Add a new token to the output that will be shown to the user."
  [output token]
  (let [last-char (last output)
        seperator (cond
                    (contains? #{\newline \(} last-char) nil
                    (contains? #{"." "!" "?" "," ";" ":" ")"} token) nil
                    :else " ")
        terminator (if (contains? #{"." "?" "!"} token)
                     "\n"
                     nil)]
    (str output seperator token terminator)))

(defn print-output!
  "Obviously needs some work, as currently all whitespace is destroyed in the
  tokenization process."
  [chain-output]
  (println (reduce concat-token chain-output)))

(defn supernatural-summaries [] (tokenize-file "texts/supernatural.txt"))

(defn two-chain
  [tokens]
  (train (new-chain 2) tokens))

(defn supernatural-plot-sample
  ([] (supernatural-plot-sample 140))
  ([n]
   (let [chain (two-chain (supernatural-summaries))]
     (-> (generate chain n)
       print-output!))))

(comment
  (def supernatural-chain (two-chain (supernatural-summaries)))
  (-> (generate supernatural-chain 140)
    print-output!)

  (def sn (tokenize-file "texts/supernatural.txt"))
  (def ks (tokenize-file "texts/kama-sutra.txt"))
  (def md (tokenize-file "texts/moby-dick.txt"))

  (def polychain
    (train-on-corpuses [sn ks md]))

  (-> (generate polychain 300)
    print-output!
    )

  (-> (generate-sentence polychain)
    print-output!)

  )
