(ns nextjournal.tafuta-tests
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [nextjournal.tafuta :as tafuta]))

(deftest parse-line-test
  (testing "parse-line"
    (is (= {:line-number 1, :occurences [[1 2]], :line "aaa"}
           (tafuta/parse-line "1;1 2:aaa")))

    (is (= {:line-number 1,
            :occurences [[34 7]],
            :line ";; This is a starting point for a scratch buffer"}
           (tafuta/parse-line "1;34 7:;; This is a starting point for a scratch buffer")))


    (is (= {:line-number 1,
            :occurences [[1 7] [9 7]],
            :line ";; This is a starting point for a scratch buffer"}
           (tafuta/parse-line "1;1 7,9 7:;; This is a starting point for a scratch buffer")))))

(deftest parse-ackmate-test
  (testing "parse-ackmate"
    (is (= '({:line-number 1,
              :occurences [[1 2]],
              :line " foo bar",
              :file "yarn.lock"}
             {:line-number 2,
              :occurences [[1 2] [5 12]],
              :line " foofoo barbar",
              :file "yarn.lock"})
           (tafuta/parse-ackmate
            ":yarn.lock\n1;1 2: foo bar\n2;1 2,5 12: foofoo barbar")))))

(deftest search-test-ag
  (testing "nextjournal.tafuta/search with ag"
    (is (= nil
           (tafuta/search "adsfadsfasdfasdfafds" "src/"))
        "empty results not working properly")
    (is (= '({:line-number 27,
              :occurences [[6 10]],
              :line "(defn parse-line",
              :file "src/nextjournal/tafuta.clj"}
             {:line-number 59,
              :occurences [[10 10]],
              :line "     (map parse-line lines)",
              :file "src/nextjournal/tafuta.clj"})
           (tafuta/search "parse-line" "src/")))
    (is (= '({:line-number 27,
              :occurences [[6 10]],
              :line "(defn parse-line",
              :file "src/nextjournal/tafuta.clj"}
             {:line-number 59,
              :occurences [[10 10]],
              :line "     (map parse-line lines)",
              :file "src/nextjournal/tafuta.clj"})
           (tafuta/search "parse-line" (io/file "src/"))))))

(deftest search-test-rg
  (testing "nextjournal.tafuta/search"
    (with-redefs [tafuta/searcher-list '("rg")]
      (is (= nil
             (tafuta/search "adsfadsfasdfasdfafds" "src/"))
          "empty results not working properly")
      (is (= '({:line-number 27,
                :occurences [[6 10]],
                :line "(defn parse-line",
                :file "src/nextjournal/tafuta.clj"}
               {:line-number 59,
                :occurences [[10 10]],
                :line "     (map parse-line lines)",
                :file "src/nextjournal/tafuta.clj"})
             (tafuta/search "parse-line" "src/")))
      (is (= '({:line-number 27,
                :occurences [[6 10]],
                :line "(defn parse-line",
                :file "src/nextjournal/tafuta.clj"}
               {:line-number 59,
                :occurences [[10 10]],
                :line "     (map parse-line lines)",
                :file "src/nextjournal/tafuta.clj"})
             (tafuta/search "parse-line" (io/file "src/")))))))

(deftest search-test-no-searcher
  (testing "nextjournal.tafuta/search"
    (with-redefs [tafuta/searcher-list '()]
      (is (thrown? clojure.lang.ExceptionInfo
                   (tafuta/search "adsfadsfasdfasdfafds" "src/")))
      "no searcher not throwing")))


(deftest search-different-args-test
  (testing "nextjournal.tafuta/search for different args"
    (is (= '({:line-number 1,
              :occurences [[16 6]],
              :line "(ns nextjournal.tafuta",
              :file "src/nextjournal/tafuta.clj"})
           (tafuta/search "tafuta" (io/file "src/nextjournal")))
        "search with dir as File not working")
    (is (= '({:line-number 1,
              :occurences [[16 6]],
              :line "(ns nextjournal.tafuta",
              :file "src/nextjournal/tafuta.clj"})
           (tafuta/search "tafuta" "src/nextjournal"))
        "search with dir as String not working")
    (is (= '({:line-number 80,
              :occurences [[6 16]],
              :line "(defn remove-duplicate",
              :file "src/nextjournal/tafuta.clj"}
             {:line-number 119,
              :occurences [[6 16]],
              :line "     (remove-duplicate :file))))",
              :file "src/nextjournal/tafuta.clj"})
           (tafuta/search "remove-duplicate" "src" {:duplicates? true}))
        "search-pattern with duplicates not working")))

(deftest search-file-test
  (testing "search-file"
    (is (= '({:path "src/nextjournal/tafuta.clj"})
           (tafuta/search-file "tafuta.clj"))
        "search-file in current directory not working")
    (is (= '({:path "src/nextjournal/tafuta.clj"}
             {:path "test/nextjournal/tafuta_tests.clj"})
           (tafuta/search-file "tafuta*clj"))
        "search-file with wildcard not working")))

(deftest search-directory-test
  (testing "search-directory"
    (is (= '({:path "src/nextjournal"} {:path "test/nextjournal"})
           (tafuta/search-directory "nextjournal"))
        "search-directory in current directory not working")
    (is (= '({:path "src/nextjournal"} {:path "test/nextjournal"})
           (tafuta/search-directory "*journal"))
        "search-with wildcard not working")))
