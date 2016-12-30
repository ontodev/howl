(ns howl.link-test
  "Test link functions."
  (:require #?(:clj  [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [howl.link :as ln]))

(defn parse-dt
  [content]
  (ln/parse-link content :start :LINK_OR_DATATYPES))

(deftest test-->iri
  (testing "handles irirefs by unpacking the iriref"
    (is (= "//foo/bar"
           (ln/->iri {}
                  [:IRIREF "<" "//foo/bar" ">"]))))
  (testing "handles PREFIXED_NAMEs by getting the prefix and returning the expanded IRI"
    (is (= "//foo/bar/baz"
           (ln/->iri {:prefix-iri {"bar" "//foo/bar/"}}
                  [:PREFIXED_NAME "bar" [:SPACES "  "] "baz"]))))
  (testing "handles LABELs by looking up the label in the environment"
    (is (= "//foo/bar/baz"
           (ln/->iri {:labels {"baz" {:iri "//foo/bar/baz"}}}
                     [:LABEL "baz"]))))
  (let [exn #?(:clj Exception :cljs js/Error)]
    (testing "Throws an exception when an expected label or prefix can't be found"
      (is (thrown? exn (ln/->iri {} [:PREFIXED_NAME "bar" [:SPACES " "] "baz"])))
      (is (thrown? exn (ln/->iri {} [:LABEL "baz"]))))
    (testing "Throws an exception when a non iriref/prefixed-name/label parse-tree is passed as the second argument"
      (is (thrown? exn (ln/->iri {} [:FOO])))
      (is (thrown? exn (ln/->iri {} [:NAME])))
      (is (thrown? exn (ln/->iri {} [:DATATYPES]))))))

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
    (is (= (ln/parse->datatype-names [:DATATYPES])
           []))
    (is (= (ln/parse->datatype-names
            [:DATATYPES " [" "LINK" " / " "LINK" " / " "LINK" "]"])
           ["LINK" "LINK" "LINK"]))
    (is (= (ln/datatype-names->parse nil)
           [:DATATYPES]))
    (is (= (ln/datatype-names->parse [])
           [:DATATYPES]))
    (is (= (ln/datatype-names->parse ["PLAIN"])
           [:DATATYPES " [" "PLAIN" "]"]))
    (is (= (ln/datatype-names->parse
            ["LINK" "LINK" "LINK"])
           [:DATATYPES " [" "LINK" " / " "LINK" " / " "LINK" "]"]))))
