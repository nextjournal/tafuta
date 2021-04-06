# Tafuta

* Tafuta - _Search_ in Swahili

A small clojure library for searching code. It shells out to [ag](https://github.com/ggreer/the_silver_searcher)
or [ripgrep](https://github.com/BurntSushi/ripgrep) and leverages the speed of those tools.
The results are returned as Clojure data. An example:

```clj
(require '[nextjournal.tafuta :as tafuta])

(tafuta/search "foo" ".")
```

For the library to work you need to have one of [ag](https://github.com/ggreer/the_silver_searcher) or
[ripgrep](https://github.com/BurntSushi/ripgrep) installed.
