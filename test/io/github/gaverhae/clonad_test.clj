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

(deftest old-syntax
  (is (= 12
         (run-plain
           (mdo [a [:pure 3]
                 b [:pure 4]
                 _ [:pure (* a b)]])))))

(deftest new-syntax
  (is (= '[:bind [:pure 3] (clojure.core/fn [a] [:bind [:pure 4] (clojure.core/fn [b] [:pure (* a b)])])]
         (macroexpand
           '(io.github.gaverhae.clonad/monad
              a :<< [:pure 3]
              b :<< [:pure 4]
              [:pure (* a b)]))))
  (is (= 12
         (run-plain
           (monad
             a :<< [:pure 3]
             b :<< [:pure 4]
             [:pure (* a b)])))))
