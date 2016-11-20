(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [howl.api :refer :all]))

;(println (nquads-to-howl (slurp "test/nquads/test1.nq")))
;(println
; (nquads-to-howl
;  (howl-to-environment (slurp "test/untyped-context/context.howl"))
;  (slurp "test/nquads/test1.nq")))

(deftest test-howl-to-nquads
  (testing "Render some HOWL to N-Quads"
    (is (= (slurp "test/nquads/test1.nq")
           (howl-to-nquads
            (slurp "test/empty-context/test1.howl"))))
    (is (= (slurp "test/empty-context/test1.howl" )
           (nquads-to-howl
            (slurp "test/nquads/test1.nq"))))
    (is (= (slurp "test/nquads/test1.nq")
           (howl-to-nquads
            (slurp "test/untyped-context/context.howl")
            (slurp "test/untyped-context/test1.howl"))))
    (is (= (slurp "test/untyped-context/test1.howl")
           (nquads-to-howl
            (howl-to-environment (slurp "test/untyped-context/context.howl"))
            (slurp "test/nquads/test1.nq"))))
    (is (= (slurp "test/nquads/test1.nq")
           (howl-to-nquads
            (slurp "test/typed-context/context.howl")
            (slurp "test/typed-context/test1.howl"))))))
