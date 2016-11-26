(ns howl.howl-test
  "Test HOWL functions."
  (:require [clojure.test :refer :all]
            [howl.howl :refer :all]))

(deftest test-group-lines
  (testing "creates singleton groups from standalone lines"
    (is (= '(("foo") ("bar") ("baz"))
           (group-lines ["foo" "bar" "baz"]))))

  (testing "groups consecutive lines that start with two spaces"
    (is (= '(("foo" "  bar") ("baz"))
           (group-lines ["foo" "  bar" "baz"])))))

; TODO: Test labels in link_test.cljc
(def bad-labels
  ["DEFAULT foo"
   "GRAPH"
   "PREFIX bar: <baz>"
   "gru TYPE integer"
   "gru LANGUAGE integer"
   "# comment"
   ">> annotation"
   "trailing space "
   "trailing colon:"
   "trailing colon arrow:>"])
