# Tafuta

* Tafuta - _Search_ in Swahili

A small clojure library for searching code. It shells out to [ag](https://github.com/ggreer/the_silver_searcher)
or [ripgrep](https://github.com/BurntSushi/ripgrep) and leverages the speed of those tools.
The results are returned as Clojure data. An example:

```clj
(require '[nextjournal.tafuta :as tafuta])

(tafuta/search "foo" ".")
=> ({:line-number 26,
     :occurences [[22 3]],
     :line "              :line \" foo bar\",",
     :file "test/nextjournal/tafuta_tests.clj"}
    {:line-number 30,
     :occurences [[22 3] [25 3]],
     :line "              :line \" foofoo barbar\",",
     :file "test/nextjournal/tafuta_tests.clj"}
    {:line-number 33,
     :occurences [[32 3] [53 3] [56 3]],
     :line
     "            \":yarn.lock\\n1;1 2: foo bar\\n2;1 2,5 12: foofoo barbar\")))))",
     :file "test/nextjournal/tafuta_tests.clj"}
    {:line-number 12,
     :occurences [[16 3]],
     :line "(tafuta/search \"foo\" \".\")",
     :file "README.md"})
```

For the library to work you need to have one of [ag](https://github.com/ggreer/the_silver_searcher) or
[ripgrep](https://github.com/BurntSushi/ripgrep) installed.
