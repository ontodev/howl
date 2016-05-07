(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.core :refer :all]))

(def test-merge "
A
  indented
   
  (that was a blank line)

B
C
  ")

(deftest test-merge-lines
  (testing "merge lines"
    (is (= (transduce
            (merge-lines "test")
            conj
            (line-seq (java.io.BufferedReader. (java.io.StringReader. test-merge))))
           [["test" 1 ""]
            ["test" 2 "A\nindented\n \n(that was a blank line)\n"]
            ["test" 7 "B"]
            ["test" 8 "C\n"]]))))

(def test-state
  {:base "http://example.com/"
   :prefixes {"ex" "http://example.com/"}
   :labels {"foo" "http://example.com/foo"}})

(deftest test-resolve-name
  (testing "absolute IRI"
    (is (= (resolve-name
            test-state
            {}
            [:WRAPPED_IRI "<" "http://example.com/foo" ">"])
           "http://example.com/foo")))
  (testing "relative IRI"
    (is (= (resolve-name
            test-state
            {}
            [:WRAPPED_IRI "<" "foo" ">"])
           "http://example.com/foo")))
  (testing "prefixed name"
    (is (= (resolve-name
            test-state
            {}
            [:PREFIXED_NAME "ex" ":" "foo"])
           "http://example.com/foo")))
  (testing "label"
    (is (= (resolve-name
            test-state
            {}
            [:LABEL "foo"])
           "http://example.com/foo"))))

