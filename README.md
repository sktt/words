This thing randomizes a text using the input text `input` from stdin. Like markov chains

It uses the first 5 chars as a start `window` and then randomly selects one
of the possible following characters (given input). Continues until 2000 chars
are there or the thing crashes because there are no possiible candidates for
next char for the given `window` causing 0 mod 0.

```sh
runhaskell main.hs < textfile
```

