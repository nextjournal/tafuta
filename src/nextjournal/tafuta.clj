(ns nextjournal.tafuta
  "A namespace for leveraging unix text search tools like \"ag\" or \"ack\"."
  (:require [babashka.process :as process]
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
  "Parses jsonlines as returned by `rg` and massages them into the clojure format
  returned by tafuta."
  [s]
  (let [jsonlines (read-jsonlines s)]
    (->> (filter #(= (:type %) "match") jsonlines)
         (map :data)
         (map (fn [match]
                {:line-number (:line_number match)
                 :occurences (mapv (fn [{:keys [start end]}] [start (- end start)]) (:submatches match))
                 :file (-> match :path :text)
                 :line (-> match :lines :text (str/replace "\n" ""))})))))

(defn search
  "Searches for `pattern` in `dir`. Uses the best available searcher installed.
  Returns nil if no results are found. Raises an error for an exit code different to 0 or 1."
  [pattern dir]
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
