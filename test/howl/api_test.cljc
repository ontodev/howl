(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.link :as link]
            [howl.api :refer :all]))

;(println (nquads-to-howl (slurp "test/nquads/test1.nq")))
;(println
; (nquads-to-howl
;  (howl-to-environment (slurp "test/typed-context/context.howl"))
;  (slurp "test/nquads/test1.nq")))
;(println
; (nquads-to-howl
;  (howl-to-environment (slurp "test/untyped-context/context.howl"))
;  (slurp "test/nquads/annotations1.nq")))

(defn compare-howl-nquads
  [context-path howl-path nquads-path]
  (reset! link/blank-node-counter 0)
  (testing (string/join " " [context-path howl-path nquads-path])
    (let [context (when context-path (slurp context-path))
          howl    (slurp howl-path)
          nquads  (slurp nquads-path)]
      (is (= nquads (howl-to-nquads context howl)))
      (is (= howl (nquads-to-howl (howl-to-environment context) nquads))))))

(deftest test-howl-to-nquads
  (compare-howl-nquads
   nil
   "test/empty-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl-nquads
   "test/untyped-context/context.howl"
   "test/untyped-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl-nquads
   "test/typed-context/context.howl"
   "test/typed-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl-nquads
   "test/untyped-context/context.howl"
   "test/untyped-context/annotations1.howl"
   "test/nquads/annotations1.nq"))

(defn round-trip
  [context-path howl-path]
  (reset! link/blank-node-counter 0)
  (testing (string/join " " [context-path howl-path])
    (let [context (when context-path (slurp context-path))
          howl    (slurp howl-path)
          nquads  (howl-to-nquads context howl)]
      (is (= howl (nquads-to-howl (howl-to-environment context) nquads))))))

(deftest test-howl-to-nquads-roundtrip
  (round-trip
   nil
   "test/empty-context/test1.howl")
  (round-trip
   "test/untyped-context/context.howl"
   "test/untyped-context/test1.howl")
  (round-trip
   "test/typed-context/context.howl"
   "test/typed-context/test1.howl")
  (round-trip
   "test/untyped-context/context.howl"
   "test/untyped-context/annotations1.howl"))
