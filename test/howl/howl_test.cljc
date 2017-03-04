(ns howl.howl-test
  "Test HOWL functions."
  (:require #?(:clj  [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [howl.howl :refer [group-lines]]))

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


(deftest test-long-statements-work
  (testing "processing a long statement does not result in a stack overflow"
    (is
     (not
      (empty?
       (instaparse.core/parses
        howl.howl/block-parser
        (str "comment: " (apply str (take 1639 (repeat '\x))))))))))
