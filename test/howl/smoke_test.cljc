(ns howl.smoke-test
  "High-level smoke-test of the top level interface and invariants."
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [clojure.string :as string]

            [howl.core :as core]
            [howl.nquads :as nquads]
            [howl.cli :refer :all]))

#?(:clj
   (do
     (deftest howl-smoke-test
       (testing "high-level smoke test for the howl parer"
         (is (let [parse (parse-file "test/test1.howl")]
               (and (seq? parse)
                    (every? map? parse)))))

       (testing "high-level smoke test for parse-lines and parse-file"
         (is (= (core/parse-lines (line-seq (clojure.java.io/reader "test/test1.howl")))
                (parse-file "test/test1.howl")))))

     (deftest howl<->nquads-smoke-test
       (testing "we can round-trip between howl and nquads"
         (is (let [f (slurp "test/test1.howl")
                   parsed (core/parse-lines (string/split-lines f))
                   e (:env (last parsed))]
               (= f (-> parsed core/blocks->nquads nquads/quads-to-howl))))))))
