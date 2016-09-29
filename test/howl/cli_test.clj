(ns howl.cli-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [howl.core :as core]
            [howl.cli :refer :all]))

(deftest howl-smoke-test
  (testing "high-level smoke test for the howl parer"
    (is (let [parse (parse-file "test/test1.howl")]
          (and (seq? parse)
               (every? map? parse)))))

  (testing "high-level smoke test for parse-lines and parse-file"
    (is (= (core/parse-lines (line-seq (clojure.java.io/reader "test/test1.howl")))
           (parse-file "test/test1.howl")))))
