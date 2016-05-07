(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.core :refer :all]))

(deftest test-locate
  (testing "file, number, line"
    (is (= (locate {:file-name "local" :line-number 3 :line "FOO"})
           "in 'local' at '3':\nFOO")))
  (testing "number and line"
    (is (= (locate {:line-number 3 :line "FOO"})
           "at '3':\nFOO")))
  (testing "number and block"
    (is (= (locate {:line-number 3 :block #{"FOO"}})
           "at '3':\n#{\"FOO\"}")))
  (testing "just state"
    (is (= (locate :just-a-state)
           ":just-a-state"))))

(def test-merge "
A
  indented
   
  (that was a blank line)

B
C
  ")

(deftest test-merge-lines
  (testing "merge line"
    (is (= (merge-line {} "1")
           {:block {:line ""}
            :line-number 1
            :merging-lines ["1"]}))
    (is (= (merge-line {:merging-lines ["1"]} "2")
           {:block {:line "1"}
            :line-number 2
            :merging-lines ["2"]}))
    (is (= (merge-line
            {:merging-lines ["1"]
             :line-number 10}
            "2")
           {:block {:line "1"}
            :line-number 11
            :merging-lines ["2"]})))
  #_(testing "merge lines"
    (is (= (transduce
            (merge-lines "test")
            conj
            (line-seq (java.io.BufferedReader. (java.io.StringReader. test-merge))))
           [["test" 1 ""]
            ["test" 2 "A\nindented\n \n(that was a blank line)\n"]
            ["test" 7 "B"]
            ["test" 8 "C\n"]]))))

(deftest test-parse
  (testing "basics"
    (is (= (parse-block {:block {:line "BASE http://foo.com"}})
           {:block
            {:line "BASE http://foo.com"
             :parse
             [:BASE_BLOCK
              "BASE"
              [:SPACES " "]
              [:BASE [:ABSOLUTE_IRI "http://foo.com"]]
              [:EOL ""]]}}))))

(deftest test-annotate
  (testing "basics"
    (is (= (annotate-block
           {:block
            {:line "BASE http://foo.com"
             :parse
             [:BASE_BLOCK
               "BASE"
               [:SPACES " "]
               [:BASE [:ABSOLUTE_IRI "http://foo.com"]]
               [:EOL ""]]}})
           {:block
            {:block-type :BASE_BLOCK
             :line "BASE http://foo.com"
             :parse
             [:BASE_BLOCK
              "BASE"
              [:SPACES " "]
              [:BASE [:ABSOLUTE_IRI "http://foo.com"]]
              [:EOL ""]]
             :base [:ABSOLUTE_IRI "http://foo.com"]
             :eol ""}}))))

(def test-state
  {:base "http://example.com/"
   :prefix-iri {"ex" "http://example.com/"}
   :label-iri {"foo" "http://example.com/foo"}})

(deftest test-expand-name
  (testing "absolute IRI"
    (is (= (expand-name
            test-state
            [:ABSOLUTE_IRI "http://example.com/foo"])
           "http://example.com/foo")))
  (testing "wrapped absolute IRI"
    (is (= (expand-name
            test-state
            [:WRAPPED_IRI "<" "http://example.com/foo" ">"])
           "http://example.com/foo")))
  (testing "wrapped relative IRI"
    (is (= (expand-name
            test-state
            [:WRAPPED_IRI "<" "foo" ">"])
           "http://example.com/foo")))
  (testing "prefixed name"
    (is (= (expand-name
            test-state
            [:PREFIXED_NAME "ex" ":" "foo"])
           "http://example.com/foo")))
  (testing "label"
    (is (= (expand-name
            test-state
            [:LABEL "foo"])
           "http://example.com/foo"))))

