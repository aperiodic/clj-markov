(ns clj-markov.test.tokenization
  (:require [clojure.core.match :refer [match]]
            [clojure.test :refer :all]
            [clj-markov.tokenization :refer :all]))

(deftest regex-match-extension
  (testing "clojure.core.match/match matches regexes against characters"
    (is (= :match (match \m
                         #"[a-z]" :match
                         :else :no-match)))))

(defn regex-predicate [regex]
  (fn [c] (-> (re-matches regex (str c)) boolean)))

(deftest character-classes
  (testing "tokenization character class regexes"
    (let [word-character? (regex-predicate word-character)
          punctuation? (regex-predicate punctuation)
          whitespace? (regex-predicate whitespace)]
      (is (word-character? \a))
      (is (word-character? \z))
      (is (word-character? \E))
      (is (word-character? \Y))
      (is (word-character? \0))
      (is (word-character? \3))
      (is (word-character? \9))
      (is (word-character? \_))
      (is (word-character? \-))
      (is (not (word-character? \tab)))
      (is (not (word-character? \!)))
      (is (not (word-character? ".9")))

      (is (punctuation? \'))
      (is (punctuation? \`))
      (is (punctuation? \"))
      (is (punctuation? \!))
      (is (punctuation? \?))
      (is (punctuation? \.))
      (is (punctuation? \:))
      (is (punctuation? \{)) (is (punctuation? \}))
      (is (punctuation? \()) (is (punctuation? \)))
      (is (punctuation? \[)) (is (punctuation? \]))
      (is (not (punctuation? \a)))
      (is (not (punctuation? \9)))
      (is (not (punctuation? \space)))
      (is (not (punctuation? "x!")))

      (is (whitespace? \space))
      (is (whitespace? \tab))
      (is (whitespace? \newline))
      (is (whitespace? \return))
      (is (not (whitespace? "x "))))))

(deftest tokenization
  (testing "tokenizing"
    (testing "does nothing on whitespace-only strings"
      (is (= () (tokenize (apply str (take 8 (cycle [\space \newline \tab \return])))))))

    (testing "a single word produces one token equal to that word"
      (let [word "updog"]
        (is (= [word] (tokenize word)))))

    (testing "multiple words produces seq of those words"
      (let [input "foo bar baz quux"]
        (is (= ["foo" "bar" "baz" "quux"] (tokenize input)))))

    (testing "punctuation marks produces a seq of those marks"
      (let [input ".,'\""]
        (is (= ["." "," "'" "\""] (tokenize input)))))

    (testing "tokenizes words and punctuation marks seperately"
      (let [input "\"Get out!\" I said to him, curtly."]
        (is (= ["\"" "Get" "out" "!" "\"" "I" "said" "to" "him" "," "curtly" "."]
               (tokenize input)))))

    (testing "tokenizes em-dashes seperately"
      (let [input "this--surely not unusual--construction should be allowed"]
        (is (= ["this" "--" "surely" "not" "unusual" "--" "construction" "should" "be" "allowed"]
               (tokenize input)))))

    (testing "doesn't tokenize an apostrophe within a word as punctuation"
      (let [input "don't tokenize that"]
        (is (= ["don't" "tokenize" "that"] (tokenize input))))

      (testing "except if it's followed by a non-word character"
        (let [input "'words are hard'"]
          (is (= ["'" "words" "are" "hard" "'"] (tokenize input))))))))
