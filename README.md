# Tafuta

**This is pre-alpha quality. Don't use it.**

* Tafuta - _Search_ in Swahili

A small clojure library for searching code, files and directories inside
a git repository. For the the pattern search of files the library shells out to
[ag](https://github.com/ggreer/the_silver_searcher) or
[ripgrep](https://github.com/BurntSushi/ripgrep) and leverages the speed of those tools.
The results are returned as Clojure data.

The API exposes two functions. In the following `directory` should always be the root of
some git repository.

`(search pattern directory)` - where `pattern` is some pattern you want to look for across tracked
git files.

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

`(search-file pattern directory)` - where `pattern` matches the files one is interested in fuzzily.

```clj
(search-file "clj")
=> ({:path "src/nextjournal/tafuta.clj"}
    {:path "test/nextjournal/tafuta_tests.clj"})
```

## Requirements

For the library to work you need to have one of [ag](https://github.com/ggreer/the_silver_searcher) or
[ripgrep](https://github.com/BurntSushi/ripgrep) installed.
