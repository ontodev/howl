(ns howl.link-test
  "Test link functions."
  (:require [clojure.test :refer :all]
            [howl.link :refer :all]))

(defn parse-dt
  [content]
  (parse-link content :start :LINK_OR_DATATYPES))

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
