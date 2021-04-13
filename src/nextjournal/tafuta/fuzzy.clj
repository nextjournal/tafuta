(ns nextjournal.tafuta.fuzzy
  (:require [clojure.string :as str]))

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
                            (bool->int (= (nth needle* (dec i))
                                          (nth haystack* (dec j))))))))))
    (let [[j score] (->> (for [j (range (inc hlen))]
                           [j (aget mat nlen j)])
                         (reduce #(if (< (second %1) (second %2)) %1 %2)))]

      {:score score
       :highlights (->> (highlights mat j needle* haystack*))})))

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
