(ns howl.link-test
  "Test link functions."
  (:require [clojure.test :refer :all]
            [howl.link :refer :all]))

(defn parse-dt
  [content]
  (parse-link content :start :LINK_OR_DATATYPES))

(deftest test-->iri
  (testing "handles irirefs by unpacking the iriref"
    (is (= "//foo/bar"
           (->iri {}
                  [:IRIREF "<" "//foo/bar" ">"]))))
  (testing "handles PREFIXED_NAMEs by getting the prefix and returning the expanded IRI"
    (is (= "//foo/bar/baz"
           (->iri {:prefix-iri {"bar" "//foo/bar/"}}
                  [:PREFIXED_NAME "bar" [:SPACES "  "] "baz"]))))
  (testing "handles LABELs by looking up the label in the environment"
    (is (= "//foo/bar/baz"
           (->iri {:labels {"baz" {:iri "//foo/bar/baz"}}}
                  [:LABEL "baz"]))))
  (testing "Throws an exception when an expected label or prefix can't be found"
    (is (thrown? Exception (->iri {} [:PREFIXED_NAME "bar" [:SPACES " "] "baz"])))
    (is (thrown? Exception (->iri {} [:LABEL "baz"]))))
  (testing "Throws an exception when a non iriref/prefixed-name/label parse-tree is passed as the second argument"
    (is (thrown? Exception (->iri {} [:FOO])))
    (is (thrown? Exception (->iri {} [:NAME])))
    (is (thrown? Exception (->iri {} [:DATATYPES])))))

(deftest test-datatypes
  (testing "parse datatypes"
    (is (= (parse-dt "")
           [:DATATYPES]))
    (is (= (parse-dt " [LINK]")
           [:DATATYPES " [" "LINK" "]"]))
    (is (= (parse-dt " [ LINK ]")
           [:DATATYPES " [ " "LINK" " ]"]))
    (is (= (parse-dt " [label with spaces]")
           [:DATATYPES " [" [:LABEL "label with spaces"] "]"]))
    (is (= (parse-dt " [LINK / LINK]")
           [:DATATYPES " [" "LINK" " / " "LINK" "]"]))
    (is (= (parse-dt " [LINK / LINK / LINK]")
           [:DATATYPES " [" "LINK" " / " "LINK" " / " "LINK" "]"]))
    (is (= (parse->datatype-names [:DATATYPES])
           []))
    (is (= (parse->datatype-names
            [:DATATYPES " [" "LINK" " / " "LINK" " / " "LINK" "]"])
           ["LINK" "LINK" "LINK"]))
    (is (= (datatype-names->parse nil)
           [:DATATYPES]))
    (is (= (datatype-names->parse [])
           [:DATATYPES]))
    (is (= (datatype-names->parse ["PLAIN"])
           [:DATATYPES " [" "PLAIN" "]"]))
    (is (= (datatype-names->parse
            ["LINK" "LINK" "LINK"])
           [:DATATYPES " [" "LINK" " / " "LINK" " / " "LINK" "]"]))))