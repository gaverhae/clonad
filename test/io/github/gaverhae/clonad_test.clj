(ns io.github.gaverhae.clonad-test
  (:require [clojure.test :refer [deftest is]]
            [io.github.gaverhae.clonad :as t :refer [match mdo monad]]))

;; The do-nothing monad, just testing the syntax

(defn run-plain
  [ma]
  (match ma
    [:pure v] v
    [:bind ma f] (let [a (run-plain ma)]
                   (run-plain (f a)))))

(defn run-plain-m
  [ma]
  (match ma
    [:m/pure v] v
    [:m/bind ma f] (let [a (run-plain-m ma)]
                     (run-plain-m (f a)))))

(deftest old-syntax
  (is (= 60
         (run-plain
           (mdo [a [:pure 3]
                 [b c] [:pure [4 5]]
                 _ [:pure (* a b c)]])))))

(deftest new-syntax
  (is (= '[:bind [:pure 3] (clojure.core/fn [a] [:bind [:pure 4] (clojure.core/fn [b] [:pure (* a b)])])]
         (macroexpand
           '(io.github.gaverhae.clonad/monad
              a :<< [:pure 3]
              b :<< [:pure 4]
              [:pure (* a b)]))))
  (is (= '[:m/bind [:m/pure 3] (clojure.core/fn [a] [:m/bind [:m/pure 4] (clojure.core/fn [b] [:m/pure (* a b)])])]
         (macroexpand
           '(io.github.gaverhae.clonad/monad :m
              a :<< [:m/pure 3]
              b :<< [:m/pure 4]
              [:m/pure (* a b)]))))
  (is (= 10
         (run-plain-m
           (monad :m
             values :<< (t/m-seq :m [[:m/pure 1] [:m/pure 2] [:m/pure 3] [:m/pure 4]])
             (t/return :m (reduce + 0 values))))))
  (is (= 60
         (run-plain
           (monad
             a :<< [:pure 3]
             [b c] :<< [:pure [4 5]]
             [:pure (* a b c)])))))
