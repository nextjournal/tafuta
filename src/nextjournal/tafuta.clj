(ns nextjournal.tafuta
  "A namespace for leveraging unix text search tools like \"ag\" or \"ack\"."
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [nextjournal.tafuta.fuzzy :as fuzzy]))

(def searcher-list '("ag" "rg"))
(def extra-arguments {"rg" ["--json"]
                      "ag" ["--ackmate"]})

(defn find-searcher
  "Returns the first searcher that is installed and supported by
  this module, nil otherwise."
  []
  (loop [[searcher & remaining] searcher-list]
    (if (nil? searcher)
      nil
      (if-let [res (try
                     (let [_res @(process/process [searcher "--version"])]
                       searcher)
                     (catch java.io.IOException _e
                       nil))]
        res
        (recur remaining)))))

(defn parse-line
  "Parses a line of the format:
  \"line-number;column-start1 pattern-length1,column-start2 pattern-length2:the-actual-line\"

  and returns it as data of the form
  {:line-number line-number
   :line line
   :occurences [[column-start1 patter-length1] [column-start2 pattern-length2]]}"
  [line]
  (let [[pattern-info line] (str/split line #":" 2)
        [line-number column-infos] (str/split pattern-info #";")
        occurences (->> (str/split column-infos #",")
                        (mapv (fn [info]
                                (let [[column-start pattern-length] (str/split info #" ")]
                                  [(Integer/parseInt column-start)
                                   (Integer/parseInt pattern-length)]))))]
    {:line-number (Integer/parseInt line-number)
     :occurences occurences
     :line line}))

(defn parse-ackmate
  "Parses a string of the format

  \":path/to/file.clj
  line-x;column-start column-end: line1-containing pattern
  line-y;column-start column-end: line2-containing pattern
  \"
  which is the ackmate format returned by \"ag\"."
  [s]
  (let [[file & lines] (str/split s #"\n")
        file (subs file 1)]
    (->>
     (map parse-line lines)
     (map #(assoc % :file file)))))

(defn read-jsonlines
  "Reads a string of jsonlines into clojure objects."
  [s]
  (->> (str/split s #"\n")
       (map #(json/parse-string % true))))

(defn parse-rg-jsonlines
  "Parses jsonlines as returned by `rg` and massages them into the clojure data."
  [s]
  (let [jsonlines (read-jsonlines s)]
    (->> (filter #(= (:type %) "match") jsonlines)
         (map :data)
         (map (fn [match]
                {:line-number (:line_number match)
                 :occurences (mapv (fn [{:keys [start end]}] [start (- end start)]) (:submatches match))
                 :file (-> match :path :text)
                 :line (-> match :lines :text (str/replace "\n" ""))})))))

(defn remove-duplicate
  "Returns a lazy sequence of the elements of coll with duplicates removed using a predicate"
  [coll pred]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen (pred f) )
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen (pred f)))))))
                 xs seen)))]
    (step coll #{})))

(defn search
  "Searches for `pattern` in `dir`. Uses the best available searcher installed.
  Returns nil if no results are found. Raises an error for an exit code different to 0 or 1.
  The default directory is the path from which the JVM was invoked.
  An optional options map can be passed as third argument. Current options
  include:
   - `duplicates?`"
  ([pattern] (search pattern "."))
  ([pattern dir]
   (let [dir (if (instance? java.io.File dir) (.getPath dir) dir)]
     (if-let [searcher (find-searcher)]
       (let [{:keys [exit out err]} @(process/process (concat [searcher pattern dir] (extra-arguments searcher))
                                                      {:out :string})]
         (case exit
           0
           (case searcher
             "ag" (->> (str/split out #"\n\n")
                       (mapcat parse-ackmate))
             "rg" (parse-rg-jsonlines out))
           1 nil
           (throw (ex-info (str searcher " exited with error code " exit) {:out out :err err}))))
       (throw (ex-info (str "No supported code search tool found!\n"
                            "Please make sure you have one of the supported search tools installed.")
                       {})))))
  ([pattern dir {:keys [duplicates?]}]
   (cond-> (search pattern dir)
     (not duplicates?)
     (remove-duplicate :file))))

(defn git-dir?
  "Returns true if the given directory is the root of a git directory."
  [dir]
  (let [git-dir (io/file dir ".git")]
    (and (.exists git-dir) (.isDirectory git-dir))))

(comment
  (git-dir? (io/file "."))
  (git-dir? (io/file "/")))

(defn git-files
  "Return all files currently under source control of given git root directory."
  [dir]
  {:pre [(git-dir? dir)]}
  (let [{:keys [exit out err]} @(process/process ["git" "ls-files"] {:dir dir :out :string})]
    (case exit
      0 (->> out
             (str/split-lines)
             (map #(io/file %)))
      (throw (ex-info (str "git ls-files exited with error code " exit) {:out out :err err})))))

(comment
  (git-files (io/file "."))
  (git-files (io/file "/")))

(def start-boost 100.0)
(def name-match-score 100.0)
(def path-match-score 10.0)
(def order-score 5.0)
(def char-penalty 1E-10)

(defn file-splitter-fn
  "Splits a file path by \"/\""
  [file]
  {:pre [(instance? java.io.File file)]}
  (->> (str/split (.getPath file) #"/")
       (remove empty?)
       vec))

(comment
  (file-splitter-fn (io/file "/foo/path/to/a/long/bar.clj"))
  (file-splitter-fn (io/file "bar/path/foo.clj")))

(defn score-pattern [pattern candidate score]
  (when-let [index (str/index-of candidate pattern)]
    (cond-> (- score (* index char-penalty))
      (= 0 index) (+ start-boost))))

(defn score-file-fn [pattern-terms file-terms]
  (let [name (last file-terms)
        path-segments (butlast file-terms)
        ;; matches in the file name
        name-matches (map (fn [pattern-term]
                            (when-let [score (score-pattern pattern-term name name-match-score)]
                              {:score score
                               :index (dec (count path-segments))}))
                          pattern-terms)
        ;; matches in the file path
        path-segment-matches (map (fn [pattern-term]
                                    (->> (map-indexed (fn [i segment]
                                                        (when-let [score (score-pattern pattern-term segment path-match-score)]
                                                          {:score score
                                                           :index i}))
                                                      path-segments)
                                         (some #(when (not= nil %1) %1))))
                                  pattern-terms)
        ;; all matches, preferring matches in the name
        matches+scores (map #(or %1 %2) name-matches path-segment-matches)
        score (->> (concat
                    (map (fn [match1 match2]
                           (when (and match1 match2)
                             ;; adding an order-score if the matchs maintain the same order
                             ;; as in the pattern
                             (update match1 :score
                                     (if (< (:index match1) (:index match2)) + -)
                                     order-score)))
                         matches+scores (rest matches+scores))
                    (list (last matches+scores)))
                   (remove nil?)
                   (reduce (fn [score match] (+ score (:score match))) 0))]
    score))

(def fuzzy-scorer (fuzzy/create-fuzzy-scorer fuzzy/standard-pattern-splitter file-splitter-fn score-file-fn))

(defn score-file [file pattern]
  {:pre [(instance? java.io.File file)]}
  {:path (str file)
   :score (fuzzy/score fuzzy-scorer pattern file)})

(comment
  (score-file (io/file "/foo/path/to/a/long/bar.clj") "foo bar" )
  (score-file (io/file "bar/path/foo.clj") "foo bar")
  (score-file (io/file "/path/barfoo.clj") "foo bar" )
  (score-file (io/file "/path/barfoo.clj") "foo")
  (score-file (io/file "bar/path/foo.clj") "foo foo")
  (score-file (io/file "foo/bar.clj") "foo")
  (score-file (io/file "foo/bar.clj") "toto")
  (score-file (io/file "foo/bar.clj") "toto titi")
  (score-file (io/file "src/nextjournal/tafuta/fuzzy.cljc") "tafuta clj"))

(defn search-file
  "Recursively searches for files matching pattern in a given directory.
  The default directory is the path from which the JVM was invoked.
  Returns the relative paths with respect to the given directory as data."
  ([pattern] (search-file pattern (io/file ".")))
  ([pattern dir] (search-file pattern dir (git-files dir)))
  ([pattern _dir files]
   (->> (map #(score-file %1 pattern) files)
        (remove #(= 0 (:score %1)))
        (sort #(compare (:score %2) (:score %1)))
        (map #(dissoc %1 :score)))))

(comment
  (search "tafuta")
  (search-file "tafuta clj")
  (search-file "manager" (io/file "/home/fv/Code/Clojure/nextjournal/"))
  (search-file "h man" (io/file "/home/fv/Code/Clojure/nextjournal/"))
  (search-file "prepl" (io/file "/home/fv/Code/Clojure/nextjournal/"))
  (search-file "schedule" (io/file "/home/fv/Code/Clojure/nextjournal/"))
  (search-file "runner" (io/file "/home/fv/Code/Clojure/nextjournal/")))
