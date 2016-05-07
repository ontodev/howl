(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.core :refer :all]))

(deftest test-location
  (testing "file, number, line"
    (is (= (location {:file-name "local" :line-number 3 :line "FOO"})
           "in 'local' at '3':\nFOO")))
  (testing "number and line"
    (is (= (location {:line-number 3 :line "FOO"})
           "at '3':\nFOO")))
  (testing "number and block"
    (is (= (location {:line-number 3 :block #{"FOO"}})
           "at '3':\n#{\"FOO\"}")))
  (testing "just state"
    (is (= (location :just-a-state)
           ":just-a-state"))))

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

