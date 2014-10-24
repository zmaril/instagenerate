(ns instagenerate.strlenc
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [run* fresh ==]]
            [clojure.core.logic.protocols :as lp])
  (:import []))

(defn -strlenc
  ([str len]
     (reify
       lp/IConstraintStep
       (-step [this s]
         (reify
           clojure.lang.IFn
           (invoke [_ s]
             (let [str (lp/walk s str)]
               (when (<= (count str) len)
                 ((l/remcg this) s))))
           lp/IRunnable
           (-runnable? [_]
             (l/ground-term? str s))))
       lp/IConstraintOp
       (-rator [_]
         `strlenc)
       (-rands [_]
         [str len])
       lp/IReifiableConstraint
       (-reifyc [_ v r s]
         `(strlenc ~str ~len))
       lp/IConstraintWatchedStores
       (-watched-stores [this] #{::subst}))))

(defn strlenc
  [str len]
  (l/cgoal (-strlenc str len)))
