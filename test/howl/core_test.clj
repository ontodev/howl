(ns howl.core-test
  (:require [clojure.test :refer :all]
            [howl.core :refer :all]))

(def test-labels
  {:labels
   {"example" "http://example.com/example"}})

(deftest test-resolve-label
  (testing "Basic cases"
    (are
     [label iri]
     (= (resolve-label test-labels label) iri)
      "example" "http://example.com/example"
      "foo" nil)))

(def test-prefixes
  {:prefixes
   {"ex" "http://example.com/"}})

(deftest test-resolve-prefixed-name
  (testing "Basic cases"
    (are
     [prefixed-name iri]
     (= (resolve-prefixed-name test-prefixes prefixed-name) iri)
      "ex:foo" "http://example.com/foo"
      "foo:ex" nil)))

(def test-base-iri
  {:base-iri "http://example.com/"})

(deftest test-resolve-relative-iri
  (testing "Absolute IRI"
    (are
     [relative absolute]
     (= (resolve-relative-iri test-base-iri relative) absolute)
      "foo" "http://example.com/foo"
      "/foo" "http://example.com/foo"
      ; TODO: "ex:foo" "http://example.com/foo"
      "http://example.com/foo" "http://example.com/foo")))

(def test-state
  (merge test-labels test-prefixes test-base-iri))

(deftest test-resolve-name
  (testing "Basic case"
    (are [name iri]
         (= (resolve-name test-state name) iri)
      "example" "http://example.com/example"
      "ex:foo" "http://example.com/foo"
      "foo" "http://example.com/foo"
      "/foo" "http://example.com/foo")))

(deftest test-add-prefix
  (testing "Basic case"
    (is (= (add-prefix {} 1 "PREFIX foo: <http://example.com/>")
           {:prefixes
            {"foo" "http://example.com/"}}))))

(deftest test-add-label
  (testing "Basic case"
    (is (= (add-label test-prefixes 1 "LABEL ex:foo: Foo")
           (merge
            test-prefixes
            {:labels
             {"Foo" "http://example.com/foo"}})))))

(def lines
  "not indented
not indented
  indented
not indented
  indented

  after blank
  indented last")

(deftest test-collect-lines
  (testing "Basic case"))

;; more
