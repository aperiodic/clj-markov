(ns clj-markov.tokenization
  (:require [clojure.core.match :as match]
            [clojure.walk :refer [postwalk]]
            [reduce-fsm :as fsm :refer [defsm-seq]]))

;;
;; Extend matching to work on regexes
;;

(defrecord RegexPattern [regex])

(defmethod match/emit-pattern java.util.regex.Pattern
  [regex]
  (RegexPattern. regex))

(defmethod match/to-source RegexPattern
  [rp value]
  `(re-find ~(:regex rp) (str ~value)))

;;
;; fsm-seq wrapper macro
;;

(defmacro fsm-seq
  "For some reason I haven't taken the time to figure out, trying to use
  symbols corresponding to local bindings as match patterns in the fsm* macros
  doesn't work, though it is supported by core.match. So, make a wrapper for
  fsm-seq that replaces any symbols that can be resolved & dereffed to regexes
  with their resolved regexes, then passes that along to reduce-fsm/fsm-seq."
  [states]
  (let [replace-regex-symbols (fn [x]
                                (cond
                                  (not (symbol? x)) x
                                  (= x '->) x
                                  (nil? (resolve x)) x
                                  (not= (type @(resolve x)) java.util.regex.Pattern) x
                                  :else @(resolve x)))]
    `(reduce-fsm/fsm-seq ~(postwalk replace-regex-symbols states))))

;;
;; Tokenization
;;

(def word-character #"[a-zA-Z0-9-_]")
(def punctuation #"['\",.:;!?\[\]\{\}\(\)]")
(def whitespace #"(?m)\s")

(def reset-token (constantly nil))
(defn reset-token-with-input [_ character _ _] (str character))
(defn add-to-token [token head-char _ _] (str token head-char))

(defn char-as-token [_ character] (str character))
(defn current-token [token _] token)

(defn word-and-apostrophe
  [both _]
  (let [[_ word apostrophe] (re-find #"([a-zA-Z0-9-_]+)(')" both)]
    (keep identity [word apostrophe])))

(defn word-and-dash
  [word-w-dash _]
  [(re-find #"[a-zA-Z0-9_]+" word-w-dash) "--"])

(def ^:private token-machine
  (fsm-seq
    [[:whitespace
      word-character -> {:action add-to-token} :word
      punctuation -> {:emit char-as-token} :whitespace
      _ -> :whitespace]
     [:word
      \' -> {:action add-to-token} :apostrophe-in-word
      \- -> {:action add-to-token} :word-or-em-dash-start
      word-character -> {:action add-to-token} :word
      whitespace -> {:emit current-token, :action reset-token} :whitespace
      punctuation -> {:emit current-token, :action reset-token-with-input} :punctuation
      _ -> {:emit current-token, :action reset-token} :whitespace]
     [:apostrophe-in-word
      word-character -> {:action add-to-token} :word
      punctuation -> {:emit word-and-apostrophe, :action reset-token} :punctuation
      _ -> {:emit word-and-apostrophe, :action reset-token} :whitespace]
     [:word-or-em-dash-start
      \- -> {:emit word-and-dash, :action reset-token} :whitespace
      word-character -> {:action add-to-token} :word
      punctuation -> {:emit current-token, :action reset-token} :punctuation
      _ -> {:emit current-token, :action reset-token} :whitespace]
     [:punctuation
      word-character -> {:emit char-as-token} :word
      whitespace -> {:emit current-token, :action reset-token} :whitespace
      punctuation -> {:emit current-token, :action reset-token-with-input} :punctuation
      _ -> :whitespace]]))

(defn tokenize
  [input]
  (-> (if-not (re-find whitespace (-> (last input) str))
        (token-machine (str input " "))
        (token-machine input))
    flatten))

(defn tokenize-file
  [filename]
  (tokenize (slurp filename)))
