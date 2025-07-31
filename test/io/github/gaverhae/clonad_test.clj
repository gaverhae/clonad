(ns io.github.gaverhae.clonad-test
  (:require [clojure.test :refer [deftest is]]
            [io.github.gaverhae.clonad :as t]))

(deftest truism
  (is (= 1 1)))
