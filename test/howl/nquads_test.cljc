(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]
            [howl.nquads :refer :all]))

(def test-state
  {:base "http://example.com/"
   :prefixes {"ex" "http://example.com/"}
   :labels {"foo" "http://example.com/foo"}})

(deftest test-resolve-name
  (testing "absolute IRI"
    (is (= (resolve-name
            test-state
            {}
            [:IRI "<" "http://example.com/foo" ">"])
           "http://example.com/foo")))
  (testing "relative IRI"
    (is (= (resolve-name
            test-state
            {}
            [:IRI "<" "foo" ">"])
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

(deftest test-format-literal
  (testing "plain literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "plain"})
           "\"plain\"")))
  (testing "language literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "English"
             :language "@en"})
           "\"English\"@en")))
  (testing "TYPE language literal"
    (is (= (format-literal
            (assoc-in
             test-state
             [:types-language "http://example.com/foo"]
             "@en")
            {:predicate [:LABEL "foo"]
             :content "English"})
           "\"English\"@en")))
  (testing "datatype literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "FOO"
             :datatype [:LABEL "foo"]})
           "\"FOO\"^^<http://example.com/foo>")))
  (testing "TYPE datatype literal"
    (is (= (format-literal
            (assoc-in
             test-state
             [:types-datatype "http://example.com/foo"]
             "http://example.com/foo")
            {:predicate [:LABEL "foo"]
             :content "FOO"})
           "\"FOO\"^^<http://example.com/foo>"))))
