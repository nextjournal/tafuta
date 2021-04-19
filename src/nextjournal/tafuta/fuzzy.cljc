(ns nextjournal.tafuta.fuzzy
  "The namespace tries to provide a general API to rank fuzzily a list of candidates,
  most likely string related, given some some search pattern"
  (:require [clojure.string :as str]))

(defn standard-pattern-splitter [s]
  (str/split s #" "))

(deftype FuzzyScorer [pattern-splitter candidate-splitter score-fn])

(defn create-fuzzy-scorer [pattern-splitter candidate-splitter score-fn]
  (->FuzzyScorer pattern-splitter candidate-splitter score-fn))

(defn score [scorer pattern object]
  ((.score-fn scorer) ((.pattern-splitter scorer) pattern) ((.candidate-splitter scorer) object)))

;; Code for a basic fuzzy searcher

(defn basic-term-score
  ([pattern candidate] (basic-term-score pattern candidate (count candidate)))
  ([pattern candidate cut-off]
   (if (< (count candidate) (count pattern))
     nil
     (loop [pa (seq pattern) ca (seq candidate) score 0]
       (cond (> score cut-off) nil
             (empty? pa) score
             (= (first pa) (first ca)) (recur (rest pa) (rest ca) score)
             :else (recur pa (rest ca) (inc score)))))))

(comment
  (basic-term-score "clj" "clojure")
  (basic-term-score "clj" "clojure" 0)
  (basic-term-score "aclj" "clojure")
  (basic-term-score "cloj" "clojure")
  )

(def match-score 100.0)
(def order-score 10.0)
(def char-panelity 2.0)

(defn basic-score-fn [pattern-terms candidate-terms]
  (let [candidates (map-indexed (fn [index candidate] {:candidate candidate :index index}) candidate-terms)
        pattern-scores (map (fn [pattern] (->> candidates
                                               (map (fn [{:keys [candidate index] :as res}]
                                                      (if-let [s (basic-term-score pattern candidate 2)]
                                                        (assoc res :score (-  (- match-score index)
                                                                              (* s char-panelity))))))
                                               (some #(when % %))))
                            pattern-terms)]
    (if (some nil? pattern-scores)
      nil
      (->> (concat
            (map (fn [match1 match2]
                   (when (and match1 match2)
                     (update match1 :score
                             (if (< (:index match1) (:index match2)) + -)
                             order-score)))
                 pattern-scores (rest pattern-scores))
            (list (last pattern-scores)))
           (remove nil?)
           (reduce (fn [score match] (+ score (:score match))) 0)))))

(comment
  (basic-score-fn ["hello" "clj"] ["hello" "clojure"])
  (basic-score-fn ["hello" "clojure"] ["hello" "clojure"])
  (basic-score-fn ["hello" "clojure"] ["hello" "world" "clojure"])
  (basic-score-fn ["hello" "world" "clojure"] ["hello" "clojure"])

  )

(def basic-scorer (create-fuzzy-scorer standard-pattern-splitter standard-pattern-splitter basic-score-fn))

(comment
  (score basic-scorer "hello clj" "hello clojure")
  (score basic-scorer "hello clojure" "hello clojure")
  (score basic-scorer "hello clojure" "hello world clojure")
  (score basic-scorer "hello world clojure" "hello clojure")
  (score basic-scorer "hello" "hello")
  (score basic-scorer "hello" "hello clojure")

  )
