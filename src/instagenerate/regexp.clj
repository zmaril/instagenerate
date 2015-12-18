(ns instagenerate.regexp
  (:refer-clojure :exclude [cat])
  (:require [instaparse.combinators :refer :all]
            [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))

(defn altnt [& keys] (apply alt (map nt keys)))
(defn hs [str] (hide (string-ci str)))
(defn alts [& strs] (apply alt (map string-ci strs)))

(defn ranged [a b]
  (map (comp str char) (range (int a) (inc (int b)))))

(def characters
  (concat
   (ranged \A \Z)
   (ranged \a \z)
   (ranged \0 \9)
   ["_" "$" " "]))

(def any-char 
  (apply alt (take 2  (map string characters))))

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
   :bracketed (cat (hs "[") (opt (string "^")) (star (altnt :range :char :escaped)) (hs "]"))
   :range     (cat (nt :char) (hs "-") (nt :char)) 
   :escaped (cat (hs  "\\") (nt :char))
   :char any-char})

(def parser
  (insta/parser regexp-map :start :regexp))

(def escaped
  {"B" Epsilon
   "s" (alts " " "\\n" "\\t")
   "t" (string "\\t")
   "n" (string "\\n")})

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
   :escaped (fn [{c :string}] (println c)(escaped c))
   :char    string
   :bracketed
   (fn [& args]
     (if (and (not (empty? args))
              (=   "^" (first args)))
       (cat (neg (apply alt (rest args))) any-char)
       (apply alt args)))
   :range (fn [{a :string} {b :string}]
            (apply alt (map string (ranged (first a) (first b)))))})

(defn regexp->parser [s]
  (as-> s $
    (parser $)
    (insta/transform transform $)
    (insta/parser {:start $} :start :start)))
