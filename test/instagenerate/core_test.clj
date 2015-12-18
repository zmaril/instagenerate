(ns instagenerate.core-test
  (:require [clojure.test       :refer :all]
            [clojure.core.logic :as l]
            [instaparse.core    :as insta]
            [instagenerate.core :as g]
            [instagenerate.regexp :refer [regexp->parser]]))

(def simple
  (insta/parser "S = 'ab' C
                 C = 'c'+"))

(deftest example-test
  (is (= (take 3 (l/run* [input output]
                   (g/instaparseo simple input output)))
         '([(\a \b \c) (:S "ab" (:C "c"))]
           [(\a \b \c \c) (:S "ab" (:C "c" "c"))]
           [(\a \b \c \c \c) (:S "ab" (:C "c" "c" "c"))]))))

(def rp
  (regexp->parser "abc?de[a-d]?"))

(deftest regexp-test
  (is (insta/failure? (rp "nope")))
  (is (= (rp "abdea") [:start "a" "b" "d" "e" "a"]))
  (is (= (rp "abcded") [:start "a" "b" "c" "d" "e" "d"]))
  (is (= (rp "abcdea") [:start "a" "b" "c" "d" "e" "a"]))
  (is (= (map (partial apply str) (g/generate-possible-strings rp))
         '("abde"
           "abcde"
           "abdea"
           "abcdea"
           "abdeb"
           "abcdeb"
           "abdec"
           "abcdec"
           "abded"
           "abcded"))))

(def mixed
  (insta/parser "S = #'ab' C
                 C = #'c'+"))

(deftest mixed-test
  (is (= (take 3 (l/run* [input output]
                   (g/instaparseo mixed input output)))
         '([(\a \b \c) (:S "ab" (:C "c"))]
           [(\a \b \c \c) (:S "ab" (:C "c" "c"))]
           [(\a \b \c \c \c) (:S "ab" (:C "c" "c" "c"))]))))
