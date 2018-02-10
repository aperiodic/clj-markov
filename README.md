# clj-markov

A clojure library for training Markov chains on text and generating output from those Markov chains.

## Usage

Clone this repo and fire up a repl.

In the repl, enter:
```clj
(require '[clj-markov.repl :refer :all])
```

This brings in the following four functions:
  * `tokenize-file`, which given a path to a file, returns a sequence of the tokens in that file, which can be used to train a markov chain.
  * `new-chain`, which generates a blank Markov chain of the given length (which defaults to two if not specified).
  * `train`, which given a Markov chain and a sequence of tokens, returns the chain after training on those tokens.
  * `generate`, which given a Markov chain and a positive integer, produces that many tokens using the Markov chain.

Typically, for a particular text, one will define a var that holds that text's tokens.
For example:

```clj
(def supernatural (tokenize-file "texts/supernatural.txt"))
```

Once the tokens are available, a Markov chain can be trained on them:

```clj
(def sn-2-chain (train (new-chain 2) supernatural))
```

Finally, output can be generated from the chain:

```clj
(-> (generate sn-2-chain 140) print-output!)
```

The `print-output!` function prints the generated tokens to standard out in a format that is easier to read than a vector literal with a bunch of strings in it.

## License

Everything besides the contents of the `texts` directory is copyright © 2014 Dan Lidral-Porter and distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

The `texts/supernatural.txt` file contains content from Wikipedia and is included under the terms of the Creative Commons Attribution Share-Alike license (see `CC-BY-SA-LICENSE` for full terms).
