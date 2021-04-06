(ns nextjournal.tafuta
  "A namespace for leveraging unix text search tools like \"ag\" or \"ack\"."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def searcher-list '("ag" "ack"))

(defn find-searcher
  "Returns the first searcher that is installed and supported by
  this module, nil otherwise."
  []
  (loop [[searcher & remaining] searcher-list]
    (if (nil? searcher)
      nil
      (if-let [res (try
                     (let [_out (shell/sh searcher "--version")]
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

(defn search
  "Searches for `pattern` in `dir`. Uses the best available searcher installed.
  Returns nil if no results are found. Raises an error for an exit code different to 0 or 1."
  [pattern dir]
  (let [dir (if (instance? java.io.File dir) (.getPath dir) dir)]
    (when-let [searcher (find-searcher)]
      (let [{:keys [exit out err]} (shell/sh searcher pattern dir "--ackmate")]
        (case exit
          0 (->> (str/split out #"\n\n")
                 (mapcat parse-ackmate))
          1 nil
          (throw (ex-info (str searcher " exited with error code " exit) {:out out :err err})))))))
