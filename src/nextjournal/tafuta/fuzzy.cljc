(ns nextjournal.tafuta.fuzzy
  "The namespace tries to provide a general API to rank fuzzily a list of candidates,
  most likely string related, given some some search pattern"
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn standard-pattern-splitter [s]
  (-> s str/lower-case (str/split #" ")))

(deftype FuzzyScorer [pattern-splitter candidate-splitter score-fn])

(defn create-fuzzy-scorer [pattern-splitter candidate-splitter score-fn]
  (->FuzzyScorer pattern-splitter candidate-splitter score-fn))

(defn score [scorer pattern candidate]
  #?(:cljs ((.-score-fn scorer) ((.-pattern-splitter scorer) pattern) ((.-candidate-splitter scorer) candidate))
     :clj ((.score-fn scorer) ((.pattern-splitter scorer) pattern) ((.candidate-splitter scorer) candidate))))

;; Code for a basic fuzzy searcher

(def highlight-comp #(< (if (seqable? %1) (first %1) %1) (if (seqable? %2) (first %2) %2)))

(defn basic-term-score
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
  (basic-term-score "clj" "clojure")
  (basic-term-score "clj" "clojure" {:cut-off 2})
  (basic-term-score "aclj" "clojure")
  (basic-term-score "cloj" "clojure")
  (basic-term-score "cloj" "clojure" {:start-index 2})
  )

(def match-score 1000.0)
(def order-score 100.0)
(def char-penalty 10.0)
(def candidate-term-penalty 1.0)
(def allowed-misses 2)

(defn calc-indexes [candidate-terms]
  (loop [[cur & remaining] candidate-terms cur-index 0 res []]
    (if (nil? cur)
      res
      (recur remaining (+ cur-index 1 (count cur)) (conj res cur-index)))))

(comment
  (calc-indexes ["foo" "bar"])
  (calc-indexes [])

  )

(defn basic-score-fn [pattern-terms candidate-terms]
  (let [candidate-indexes (calc-indexes candidate-terms)
        candidates (->> (map-indexed (fn [index candidate] {:candidate candidate :pos-index index}) candidate-terms)
                        (map (fn [index candidate] (assoc candidate :index index)) candidate-indexes))
        pattern-scores (map (fn [pattern] (->> candidates
                                               (map (fn [{:keys [candidate pos-index index] :as res}]
                                                      (if-let [score-map (basic-term-score pattern candidate
                                                                                           {:cut-off allowed-misses
                                                                                            :start-index index})]
                                                        (merge res
                                                               (update score-map :score #(-  (- match-score pos-index)
                                                                                             (* %1 char-penalty)))))))
                                               (some #(when % %))))
                            pattern-terms)
        non-matched-candidates (seq (set/difference (set (range (count candidate-terms)))
                                                    (set (map :pos-index pattern-scores))))]
    (if (some nil? pattern-scores)
      nil
      (let [res (->> (concat
                      (map (fn [match1 match2]
                             (when (and match1 match2)
                               (update match1 :score
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
  (score basic-scorer "clj" "clj clj")
  (score basic-scorer "clj" "clj-kondo bar foo foo foo")
  (score basic-scorer "clj" "foo clj-kondo bar foo foo ")
  (score basic-scorer "clj foo" "foo cl0j-kondo")
  (score basic-scorer "clj" "a clojure")
  (score basic-scorer "clj" "a very good clojure")

  )
