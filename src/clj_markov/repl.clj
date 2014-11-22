(ns clj-markov.repl
  (:require [clj-markov.generation :refer [generate]]
            [clj-markov.tokenization :refer [tokenize-file]]
            [clj-markov.training :refer [train]]))

(defn print-output!
  "Obviously needs some work, as currently all whitespace is destroyed in the
  tokenization process."
  [chain-output]
  (println (reduce #(case %2 
                      "." (str %1 %2 "\n") 
                      (",",";",":",")") (str %1 %2) 
                      (str %1 " " %2)) 

(defn supernatural-summaries [] (tokenize-file "texts/supernatural.txt"))

(defn two-chain
  [tokens]
  (train tokens {:length 2}))

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
  )
