(ns instagenerate.regexp
  (:refer-clojure :exclude [cat])
  (:require [instaparse.combinators :refer :all]
            [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))

(defn altnt [& keys] (apply alt (map nt keys)))
(defn hs [str] (hide (string-ci str)))

(defn ranged [a b]
  (map (comp str char) (range (int a) (inc (int b)))))

(def characters
  (concat
   (ranged \A \Z)
   (ranged \a \z)
   (ranged \0 \9)
   ["_" "$"]))

(def regexp-map
  {:regexp (cat  (nt :term) (star (cat (hs "|") (nt :term))))
   :term   (star (nt :factor))
   :factor (cat  (nt :base)
                 (opt (alt (string "*")
                           (string "?")
                           (string "+"))))
   :base   (alt (nt :char) (nt :grouped) (nt :escaped)
                (nt :bracketed))
   :grouped   (cat (hs "(") (nt :regexp) (hs ")"))
   :bracketed (cat (hs "[") (star (altnt :range :char)) (hs "]"))
   :range     (cat (nt :char) (hs "-") (nt :char)) 
   :escaped (cat (hs  "\\") (nt :char))
   :char (apply alt (map string characters))})

(def parser
  (insta/parser regexp-map :start :regexp))

(def escaped
  {"B" Epsilon
   })
(def transform
  {:regexp alt
   :term   cat
   :factor
   (fn
     ([arg]   arg)
     ([arg modifier]
      (case modifier
        "*" (star arg)
        "?" (opt arg)
        "+" (plus arg))))
   :base    cat
   :grouped cat
   :escaped (fn [{c :string}] (escaped c))
   :char    string
   :bracketed alt
   :range (fn [{a :string} {b :string}]
            (apply alt (map string (ranged (first a) (first b)))))})

(defn regexp->parser [s]
  (as-> s $
    (parser $)
    (insta/transform transform $)
    (insta/parser {:start $} :start :start)))
