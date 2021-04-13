(ns nextjournal.tafuta.fuzzy
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn highlight
  "Given a string and a list of indices to highlight. Returns
  highlighted hiccup of the string.
  (highlight \"clojure\" [0 [2 4]]) => [[:em \"c\"] \"l\" [:em \"oj\"] \"ure\"
  The highlights are either simple indices or a subsection in form of a pair."
  [s highlights]
  (let [get-beginning (fn [x] (if (vector? x) (first x) x))
        get-end (fn [x] (if (vector? x) (dec (second x)) x))
        emphasized (mapv (fn [x] [:em (if (vector? x)
                                        (subs s (first x) (second x))
                                        (subs s x (inc x)))]) highlights)
        unemphasized (mapv (fn [x y] (subs s (inc (get-end x)) (get-beginning y)))
                           (cons -1 highlights)
                           (concat highlights [(count s)]))]
    (->> (interleave unemphasized (conj emphasized ""))
         (remove #(= % "")))))

(defn highlights [mat start needle haystack]
  (loop [highlighted [] i (count needle) j start]
    (cond (or (= i 0) (= j 0))
          (vec (reverse highlighted))
          (= (dec (aget mat i j)) (aget mat (dec i) j))
          (recur highlighted (dec i) j)
          (= (dec (aget mat i j)) (aget mat i (dec j)))
          (recur highlighted i (dec j))
          :else
          (recur (cond-> highlighted
                   (= (nth needle (dec i)) (nth haystack (dec j)))
                   (conj (dec j)))
                 (dec i)
                 (dec j)))))

(defn bool->int [b]
  (if b 0 1))

;; modified levenshtein distance for approximate substring matching as described in
;; https://en.wikipedia.org/wiki/Approximate_string_matching

(defn fuzzy-score [needle haystack]
  (let [nlen (count needle)
        hlen (count haystack)
        mat (make-array Long/TYPE (inc nlen) (inc hlen))
        needle* (str/lower-case needle)
        haystack* (str/lower-case haystack)]
    (doseq [i (range (inc nlen))]
      (doseq [j (range (inc hlen))]
        (cond (= i 0) (aset mat i j 0)
              (= j 0) (aset mat i j i)
              :else
              (aset mat i j
                    (min (inc (aget mat (dec i) j))
                         (inc (aget mat i (dec j)))
                         (+ (aget mat (dec i) (dec j))
                            (if (= (nth needle* (dec i))
                                   (nth haystack* (dec j)))
                              0 2)))))))
    (let [[j score] (->> (for [j (range (inc hlen))]
                           [j (aget mat nlen j)])
                         (reduce #(if (< (second %1) (second %2)) %1 %2)))]

      {:score score
       :highlights (highlights mat j needle* haystack*)})))

(comment
  (fuzzy-score "clj" "clojure")
  (fuzzy-score "clj" "cool jerseys")
  (fuzzy-score "clj" "cool left jerseys")
  (highlight "cool jerseys" (:highlights (fuzzy-score "clj" "cool jerseys")))
  (fuzzy-score "clj" "aclojure")
  (fuzzy-score "agda" "CI")
  (fuzzy-score "agda" "Agda Environment")
  (fuzzy-score "agda" "foo Agda Environment")
  (fuzzy-score "foo" "bar")
  )


(defn highlights2 [mat start needle _haystack]
  (loop [highlighted [] i start j (count needle)]
    (cond (or (= i 0) (= j 0))
          (vec (reverse highlighted))
          (and (not= -1 (aget mat (dec i) j)) (= (dec (aget mat i j)) (aget mat (dec i) j)))
          (recur highlighted (dec i) j)
          :else
          (recur (conj highlighted (dec i)) (dec i) (dec j)))))


(defn fuzzy-score2 [needle haystack]
  (let [nlen (count needle)
        hlen (count haystack)
        mat (make-array Long/TYPE (inc hlen) (inc nlen))
        needle* (str/lower-case needle)
        haystack* (str/lower-case haystack)]
    (doseq [j (range (inc nlen))]
      (doseq [i (range (inc hlen))]
        (cond (and (= i 0) (= j 0)) (aset mat i j 0)
              (< i j) (aset mat i j -1)
              (= j 0) (aset mat i j i)
              :else
              (aset mat i j
                    (let [tmp (aget mat (dec i) j)
                          res (if (= tmp -1) -1 (inc tmp))
                          tmp (aget mat (dec i) (dec j))
                          res (if (and (= (nth needle* (dec j))
                                          (nth haystack* (dec i)))
                                       (not= tmp -1))
                                (if (not= res -1)
                                  (min res tmp)
                                  tmp)
                                res)]
                      res)))))
    (if-let [results (->> (for [i (range (inc hlen))]
                            [i (aget mat i nlen)])
                          (filter #(not= -1 (second %1)))
                          seq)]
      (let [[j score] (reduce #(if (< (second %1) (second %2)) %1 %2) results)]
        {:score score
         :highlights (highlights2 mat j needle* haystack*)})
      nil)))

(comment
  (fuzzy-score2 "foo" "bar")
  (fuzzy-score2 "clj" "clojure")
  (fuzzy-score2 "clj" "cool jerseys")
  (fuzzy-score2 "clj" "cool left jerseys")
  (highlight "cool jerseys" (:highlights (fuzzy-score2 "clj" "cool jerseys")))
  (fuzzy-score2 "clj" "aclojure")
  (fuzzy-score2 "clj" "a clojure")
  (fuzzy-score2 "data" "Data objects")
  (fuzzy-score2 "data" "the Data objects")
  (fuzzy-score2 "clj data" "clj data")
  (fuzzy-score2 "clj data" "clojure data")
  (fuzzy-score2 "clj data" "cljodata")
  (fuzzy-score2 "clj data" "clojure the data")
  (fuzzy-score2 "clj data" "the clojure data")
  (fuzzy-score2 "agda" "Agda Environment")
  (fuzzy-score2 "agda" "foo Agda Environment")
  )

(def char-panelty (* Double/MIN_VALUE 2))
(def word-panelty Double/MIN_VALUE)

(defn fuzzy-match [needle haystack]
  (let [needles (str/split needle #" ")
        haystacks (str/split haystack #" ")
        {:keys [score] :as res} (fuzzy-score2 needle haystack)]
    (if (>= (count haystacks) (count needles))
      (let [haystack-combos (combo/combinations haystacks (count needles))
            haystack-combos-score (map (fn [haystacks]
                                         (->> (map (fn [needle haystack] (fuzzy-score2 needle haystack)) needles haystacks)
                                              (reduce (fn [res {:keys [score]}] (+ res score)) 0)))
                                       haystack-combos)]
        haystack-combos-score)
      res)))

(comment
  (fuzzy-score2 "foo" "bar")
  (fuzzy-score2 "clj" "clojure")
  (fuzzy-score2 "clj" "cool jerseys")
  (fuzzy-score2 "clj" "cool left jerseys")
  (highlight "cool jerseys" (:highlights (fuzzy-score2 "clj" "cool jerseys")))
  (fuzzy-score2 "clj" "aclojure")
  (fuzzy-score2 "clj" "a clojure")
  (fuzzy-score2 "data" "Data objects")
  (fuzzy-match "data" "the Data objects")
  (fuzzy-score2 "clj data" "clj data")
  (fuzzy-score2 "clj data" "clojure data")
  (fuzzy-score2 "clj data" "cljodata")
  (fuzzy-score2 "clj data" "clojure the data")
  (fuzzy-score2 "clj data" "the clojure data")
  (fuzzy-score2 "agda" "Agda Environment")
  (fuzzy-score2 "agda" "foo Agda Environment")
  )
