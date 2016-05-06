(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [howl.core :refer :all]))

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
