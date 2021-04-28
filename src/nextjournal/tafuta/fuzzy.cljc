(ns nextjournal.tafuta.fuzzy
  "The namespace tries to provide a general API to rank fuzzily a list of candidates,
  most likely string related, given some some search pattern"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn standard-pattern-splitter [s]
  (-> s str/lower-case (str/split #" ")))

(deftype FuzzyScorer [pattern-splitter candidate-splitter score-fn])

(defn create-fuzzy-scorer
  "A fuzzy scorer takes a pattern-splitter, a function preprocessing
  the search pattern, a candidate-splitter, a function preprocessing
  the candidate and a scoring function that takes the preprocessed
  pattern and candidate terms and returns data, including some sort of score."
  [pattern-splitter candidate-splitter score-fn]
  (->FuzzyScorer pattern-splitter candidate-splitter score-fn))

(defn score [scorer pattern candidate]
  #?(:cljs ((.-score-fn scorer) ((.-pattern-splitter scorer) pattern) ((.-candidate-splitter scorer) candidate))
     :clj ((.score-fn scorer) ((.pattern-splitter scorer) pattern) ((.candidate-splitter scorer) candidate))))

;;;; Code for a basic fuzzy scorer
;;
;; The idea of the basic fuzzy scorer is to first split the pattern and the candidate
;; by spaces, i.e. pattern "foo bar" and candidate "the foo bar cafe" become
;; ["foo" "bar"] and ["the" "foo" "bar" "cafe"]. These pattern and candidate terms
;; are than pairwise compared giving each pair some score. The best scores are then
;; recombined giving some penalty for certain irregularities like extra characters in
;; between, extra non matched terms in the candidate, different order of pattern terms
;; matched in candidate terms.

(defn highlight-comp
  "Compares an index (single integer) or an interval (a pair of integers).
  In case of an interval the beginning is taken for comparison."
  [highlight1 highlight2]
  (< (if (seqable? highlight1) (first highlight1) highlight1)
     (if (seqable? highlight2) (first highlight2) highlight2)))

(defn basic-term-score
  "Scores a pattern term and a candidate term. All characters of `pattern` must be
  matched in `candidate`. The function allows for certain number of extra characters
  in `candidate` between the characters of `pattern` which can be finetuned via the
  options map with `:cut-off`. An exception is a complete match without \"holes\".

  Examples:
  * (basic-term-score \"clj\" \"cloojure\" {:cut-off 2});; => {:score 3, :highlights [0 1 4]}
  * (basic-term-score \"clj\" \"cloojure\" {:cut-off 1});; => nil
  * (basic-term-score \"clj\" \"aaaaacljure\" {:cut-off 1});; => {:score 1, :highlights [[5 8]]}

  Another optional parameter is `:start-index` which shifts the highlight indices.

  IMPORTANT: This function returns a low score for a good match. Good to use for penalty when
  calculating a large score for matches."
  ([pattern candidate] (basic-term-score pattern candidate {:cut-off (count candidate) :start-index 0}))
  ([pattern candidate {:keys [cut-off start-index] :or {cut-off 0 start-index 0}}]
   (let [i (str/index-of candidate pattern)]
     (cond (< (count candidate) (count pattern)) nil
           i {:score (if (= 0 i) 0 1) :highlights [[(+ i start-index) (+ i start-index (count pattern))]]}
           :else (loop [pa (seq pattern) ca (seq candidate) i start-index score 0 highlights []]
                   (cond (> score cut-off) nil
                         (empty? pa) {:score (inc score) :highlights highlights}
                         (= (first pa) (first ca)) (recur (rest pa) (rest ca) (inc i) score (conj highlights i))
                         :else (recur pa (rest ca) (inc i) (inc score) highlights)))))))

(comment
  (basic-term-score "clj" "cloojure" {:cut-off 2});; => {:score 3, :highlights [0 1 4]}
  (basic-term-score "clj" "cloojure" {:cut-off 1});; => nil
  (basic-term-score "clj" "aaaaacljure" {:cut-off 1});; => {:score 1, :highlights [[5 8]]}
  (basic-term-score "aclj" "clojure")
  (basic-term-score "cloj" "clojure"))

(def match-score 1000.0)
(def order-score 100.0)
(def char-penalty 10.0)
(def candidate-term-penalty 1.0)
(def allowed-misses 2)

(defn calc-indexes
  "Calculates the respective starting indices of a sequence of terms if
  they would be joined by a space."
  [candidate-terms]
  (loop [[cur & remaining] candidate-terms cur-index 0 res []]
    (if (nil? cur)
      res
      (recur remaining (+ cur-index 1 (count cur)) (conj res cur-index)))))

(comment
  (calc-indexes ["foo" "bar"])
  (calc-indexes []))

(defn basic-score-fn [pattern-terms candidate-terms]
  (let [candidate-indexes (calc-indexes candidate-terms)
        ;; associating a positional index (in the candidate) and the starting indices of the candidate terms
        candidates (->> (map-indexed (fn [index candidate] {:candidate candidate :pos-index index}) candidate-terms)
                        (map (fn [index candidate] (assoc candidate :index index)) candidate-indexes))
        pattern-scores (map (fn [pattern] (->> candidates
                                               (map (fn [{:keys [candidate pos-index index] :as res}]
                                                      (if-let [score-map (basic-term-score pattern candidate
                                                                                           {:cut-off allowed-misses
                                                                                            :start-index index})]
                                                        (merge res
                                                               ;; add a match-score minus some positional info (earlier
                                                               ;; terms a weight more heavily) minus character penalty
                                                               ;; as returned by basic-term-score
                                                               (update score-map :score #(-  (- match-score pos-index)
                                                                                             (* %1 char-penalty)))))))
                                               (some #(when % %)))) ;; currently taking the first candidate match
                            pattern-terms)
        non-matched-candidates (seq (set/difference (set (range (count candidate-terms)))
                                                    (set (map :pos-index pattern-scores))))]
    ;; if any pattern term doesn't have a match we return nil
    (if (some nil? pattern-scores)
      nil
      (let [res (->> (concat
                      (map (fn [match1 match2]
                             (when (and match1 match2)
                               (update match1 :score
                                       ;; adding an order-score if the matchs maintain the same order
                                       ;; as in the pattern
                                       (if (< (:pos-index match1) (:pos-index match2)) + -)
                                       order-score)))
                           pattern-scores (rest pattern-scores))
                      (list (last pattern-scores)))
                     (remove nil?)
                     (reduce (fn [{:keys [score highlights]} match]
                               {:score (+ score (:score match))
                                :highlights (concat highlights (:highlights match))})
                             {:score 0
                              :highlights []}))]
        (-> res
            ;; subtracting a penalty for non matched candidates based on the non matched terms index
            (update :score #(- % (* (->> non-matched-candidates
                                         (map (fn [index] (-  (count candidate-terms) index)))
                                         (reduce +))
                                    candidate-term-penalty)))
            (update :highlights #(sort highlight-comp %)))))))


(comment
  (basic-score-fn ["hello" "clj"] ["hello" "clojure"])
  (basic-score-fn ["hello" "clojure"] ["hello" "clojure"])
  (basic-score-fn ["hello" "clojure"] ["hello" "world" "clojure"])
  (basic-score-fn ["hello" "world" "clojure"] ["hello" "clojure"])

  )

(def basic-scorer (create-fuzzy-scorer standard-pattern-splitter standard-pattern-splitter basic-score-fn))

(comment
  (score basic-scorer "h" "aha")
  (score basic-scorer "h" "haskell two")
  (score basic-scorer "hello clj" "hello clj")
  (score basic-scorer "hello clj" "hello Clojure")
  (score basic-scorer "clj" "clj")
  (score basic-scorer "clj" "clj-kondo")
  (score basic-scorer "clj" "ci: clj-kondo")
  (score basic-scorer "clj" "clj-kondo minimal repro template")
  (score basic-scorer "clj" "clj clj")
  (score basic-scorer "clj" "clj-kondo bar foo foo foo")
  (score basic-scorer "clj" "foo clj-kondo bar foo foo ")
  (score basic-scorer "clj foo" "foo cl0j-kondo")
  (score basic-scorer "clj" "a clojure")
  (score basic-scorer "clj" "a very good clojure")

  )
