(defproject clj-markov "0.1.0-SNAPSHOT"
  :description "Markov chain training & generation."
  :url "https://github.com/aperiodic/clj-markov"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx2G"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [reduce-fsm "0.1.2"]])
