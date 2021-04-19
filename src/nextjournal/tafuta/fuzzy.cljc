(ns nextjournal.tafuta.fuzzy
  "The namespace tries to provide a general API to rank fuzzily a list of objects,
  most likely string related, given some some search pattern"
  (:require [clojure.string :as str]))

(defn standard-pattern-splitter [s]
  (str/split s #" "))

(deftype FuzzyScorer [pattern-splitter object-splitter score-fn])

(defn create-fuzzy-scorer [pattern-splitter object-splitter score-fn]
  (->FuzzyScorer pattern-splitter object-splitter score-fn))

(defn score [scorer pattern object]
  ((.score-fn scorer) ((.pattern-splitter scorer) pattern) ((.object-splitter scorer) object)))


(comment
  file-search
  path and name
  rank name higher than path
  and beginning better than end in name
  as well then path splits

  title-search
  terms
  earlier terms better

  strings -> vector of splits -> scoring function
  scoring function takes
  )
