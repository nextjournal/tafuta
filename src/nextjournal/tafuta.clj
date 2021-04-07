(ns nextjournal.tafuta
  "A namespace for leveraging unix text search tools like \"ag\" or \"ack\"."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jsonista.core :as json]))

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
       (map #(json/read-value % json/keyword-keys-object-mapper))))

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
       (let [{:keys [exit out err]} @(process/process (concat [searcher pattern dir] (extra-arguments searcher)))]
         (case exit
           0 (let [out (slurp out)]
               (case searcher
                 "ag" (->> (str/split out #"\n\n")
                           (mapcat parse-ackmate))
                 "rg" (parse-rg-jsonlines out)))
           1 nil
           (throw (ex-info (str searcher " exited with error code " exit) {:out out :err err}))))
       (throw (ex-info (str "No supported code search tool found!\n"
                            "Please make sure you have one of the supported search tools installed.")
                       {})))))
  ([pattern dir {:keys [duplicates?]}]
   (cond-> (search pattern dir)
     (not duplicates?)
     (remove-duplicate :file))))

(defn try-convert-to-regex
  "Tries to convert a string to a regex, returns the string if not a valid
  Java regex."
  [s]
  (try
    (re-pattern s)
    (catch java.util.regex.PatternSyntaxException _e
      s)))

(defn git-dir?
  "Returns true if the given directory is the root of a git directory."
  [dir]
  (let [git-dir (io/file dir ".git")]
    (and (.exists git-dir) (.isDirectory git-dir))))

(comment
  (git-dir? (io/file "."))
  (git-dir? (io/file "/"))
  )

(defn git-files
  "Return all files currently under source control of given git root directory."
  [dir]
  {:pre [(git-dir? dir)]}
  (let [{:keys [exit out err]} @(process/process ["git" "ls-files"] {:dir dir})]
    (case exit
      0 (->> (slurp out)
             (str/split-lines)
             (map #(io/file %)))
      (throw (ex-info (str "git ls-files exited with error code " exit) {:out out :err err})))))

(comment
  (git-files (io/file "."))
  (git-files (io/file "/"))
  )

(def non-escaped-stars-regex #"(?<!\\)\*")

(defn replace-non-escaped-stars
  "Replaces non escaped stars with \".*\"."
  [pat]
  (str/replace pat non-escaped-stars-regex ".*"))

(comment
  (replace-non-escaped-stars "\\*123")
  (replace-non-escaped-stars "\\*123*")
  )

(defn search-file
  "Recursively searches for files matching pattern in a given directory.
  The default directory is the path from which the JVM was invoked.
  Returns the relative paths with respect to the given directory as data.
  A non escaped star is interpreted as a wildcard character."
  ([pattern] (search-file pattern (io/file ".")))
  ([pattern dir]
   (let [files (git-files dir)
         regex (-> pattern replace-non-escaped-stars try-convert-to-regex)
         results (cond
                   (empty? pattern)
                   files

                   (= (type regex) java.lang.String)
                   (filter #(str/includes? (.getName %) pattern) files)

                   :else
                   (filter #(re-matches regex (.getName %)) files))]
     (map (fn [file] {:path (str file)}) results))))

(comment
  (search-file "*clj")
  (search-file "taf*.clj")
  )

(defn git-dirs
  "Return all directories currently under source control of given git root directory."
  [dir]
  {:pre [(git-dir? dir)]}
  (let [{:keys [exit out err]} @(process/process ["git" "ls-tree" "-d" "-r" "HEAD" "--name-only"] {:dir dir})]
    (case exit
      0 (->> (slurp out)
             (str/split-lines)
             (map #(io/file %)))
      (throw (ex-info (str "git ls-files exited with error code " exit) {:out out :err err})))))

(comment
  (git-dirs (io/file "."))
  (git-dirs (io/file "/"))
  )

(defn search-directory
  "Recursively searches for directories matching pattern in a given directory.
  The default directory is the path from which the JVM was invoked.
  Returns the relative paths with respect to the given directory as data.
  A non escaped star is interpreted as a wildcard character."
  ([pattern] (search-directory pattern (io/file ".")))
  ([pattern dir]
   (let [dirs (git-dirs dir)
         regex (-> pattern replace-non-escaped-stars try-convert-to-regex)
         results (cond
                   (empty? pattern)
                   dirs

                   (= (type regex) java.lang.String)
                   (filter #(str/includes? (.getName %) pattern) dirs)

                   :else
                   (filter #(re-matches regex (.getName %)) dirs))]
     (map (fn [file] {:path (str file)}) results))))

(comment
  (search-directory "nextjournal")
  (search-directory "*editor*")
  )
