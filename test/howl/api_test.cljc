(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [howl.api :refer :all]))

;(println (howl-to-nquads (slurp "test/empty-context/test1.howl")))

(deftest test-howl-to-nquads
  (testing "Render some HOWL to N-Quads"
    (is (= (slurp "test/nquads/test1.nq")
           (howl-to-nquads (slurp "test/empty-context/test1.howl"))))))
