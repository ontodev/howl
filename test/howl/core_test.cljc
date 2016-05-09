(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.core :refer :all]))

(deftest test-locate
  (testing "file, number, line"
    (is (= (locate {:file-name "local" :line-number 3 :line "FOO"})
           "in 'local' at line 3:\nFOO")))
  (testing "number and line"
    (is (= (locate {:line-number 3 :line "FOO"})
           "at line 3:\nFOO")))
  (testing "number and block"
    (is (= (locate {:line-number 3 :block #{"FOO"}})
           "at line 3:\n#{\"FOO\"}")))
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
            {:line-number 10
             :merging-lines ["1"]}
            "2")
           {:block {:line "1"}
            :line-number 11
            :merging-lines ["2"]})))
  (testing "merge error"
    (is (= (merge-line
            {:line-number 10
             :merging-lines ["1"]}
            nil)
           {:errors ["Line 'null' is not a string"]
            :line-number 10
            :merging-lines ["1"]}))))

(deftest test-parse
  (testing "empty"
    (is (= (parse-block {:empty nil})
           {:empty nil})))
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

(deftest test-composition
  (testing "one line"
    (is (= (-> (merge-line {:merging-lines ["BASE http://foo.com"]} "2")
               parse-block
               annotate-block)
           {:merging-lines ["2"]
            :line-number 2
            :block
            {:block-type :BASE_BLOCK
             :line "BASE http://foo.com"
             :parse
             [:BASE_BLOCK
              "BASE"
              [:SPACES " "]
              [:BASE [:ABSOLUTE_IRI "http://foo.com"]]
              [:EOL ""]]
             :base [:ABSOLUTE_IRI "http://foo.com"]
             :eol ""}})))
  (testing "multiple lines"
    (is (= (process-lines
            (fn [state line]
              (-> (merge-line state line)
                  parse-block
                  annotate-block))
            {}
            ["BASE http://foo.com" "2" "3: 4\n  5"])
           [{:block-type :BLANK_BLOCK
             :line ""
             :parse [:BLANK_BLOCK [:EOL ""]]
             :eol ""}
            {:block-type :BASE_BLOCK
             :line "BASE http://foo.com"
             :parse
             [:BASE_BLOCK
              "BASE"
              [:SPACES " "]
              [:BASE [:ABSOLUTE_IRI "http://foo.com"]]
              [:EOL ""]]
             :base [:ABSOLUTE_IRI "http://foo.com"]
             :eol ""}
            {:block-type :SUBJECT_BLOCK
             :line "2"
             :parse [:SUBJECT_BLOCK [:SUBJECT [:LABEL "2"]] [:EOL ""]]
             :subject [:LABEL "2"]
             :eol ""}
            {:block-type :LITERAL_BLOCK
             :line "3: 4\n  5"
             :parse [:LITERAL_BLOCK
                     [:ARROWS "" ""]
                     [:PREDICATE [:LABEL "3"]]
                     [:COLON "" ":" " "]
                     [:LITERAL "4\n  5"]
                     [:EOL ""]]
             :arrows ""
             :predicate [:LABEL "3"]
             :content "4\n  5"
             :eol ""}
            ])))
  (testing "error"
    (is (thrown-with-msg?
         Exception
         #"Line '1234' is not a string at line 1:"
         (process-lines
            (fn [state line]
              (-> (merge-line state line)
                  parse-block
                  annotate-block))
            {}
            ["BASE http://foo.com" 1234])))))

(def test-state
  {:base "http://example.com/"
   :prefix-iri {"ex" "http://example.com/"}
   :label-iri {"foo" "http://example.com/foo"}})

(deftest test-expand
  (testing "absolute IRI"
    (is (= (expand
            test-state
            [:ABSOLUTE_IRI "http://example.com/foo"])
           "http://example.com/foo")))
  (testing "wrapped absolute IRI"
    (is (= (expand
            test-state
            [:WRAPPED_IRI "<" "http://example.com/foo" ">"])
           "http://example.com/foo")))
  (testing "wrapped relative IRI"
    (is (= (expand
            test-state
            [:WRAPPED_IRI "<" "foo" ">"])
           "http://example.com/foo")))
  (testing "prefixed name"
    (is (= (expand
            test-state
            [:PREFIXED_NAME "ex" ":" "foo"])
           "http://example.com/foo")))
  (testing "label"
    (is (= (expand
            test-state
            [:LABEL "foo"])
           "http://example.com/foo"))))

(deftest test-expand-names
  (testing "prefix"
    (is (= (expand-names
            {:block
             {:block-type :PREFIX_BLOCK
              :prefix "rdfs"
              :prefixed [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#"]
              }})
           {:prefix-iri {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"}
            :iri-prefix {"http://www.w3.org/2000/01/rdf-schema#" "rdfs"}
            :block
            {:block-type :PREFIX_BLOCK
             :prefix "rdfs"
             :prefixed [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#"]}})))
  (testing "type"
    (is (= (expand-names
            {
             :label-iri {"2" "http://foo.com/2"}
             :block
             {:block-type :TYPE_BLOCK
              :predicate [:LABEL "2"]
              :type [:LABEL "2"]}})
           {:label-iri {"2" "http://foo.com/2"}
            :iri-type {"http://foo.com/2" {:type "http://foo.com/2"}}
            :block
            {:block-type :TYPE_BLOCK
             :predicate [:ABSOLUTE_IRI "http://foo.com/2"]
             :type [:ABSOLUTE_IRI "http://foo.com/2"]}})))
  (testing "subject"
    (is (= (expand-names
            {:label-iri {"2" "http://foo.com/2"}
             :block
             {:block-type :SUBJECT_BLOCK
              :subject [:LABEL "2"]}})
           {:label-iri {"2" "http://foo.com/2"}
            :current-subject "http://foo.com/2"
            :block
            {:block-type :SUBJECT_BLOCK
             :subject [:ABSOLUTE_IRI "http://foo.com/2"]}})))
  (testing "rdfs:label"
    (is (= (expand-names
            {:prefix-iri {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"}
             :current-subject "http://foo.com/2"
             :block
             {:block-type :LITERAL_BLOCK
              :predicate [:PREFIXED_NAME "rdfs" ":" "label"]
              :content "2"
              :type [:PREFIXED_NAME "rdfs" ":" "integer"]}})
           {:prefix-iri {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"}
            :label-iri {"2" "http://foo.com/2"}
            :iri-label {"http://foo.com/2" "2"}
            :current-subject "http://foo.com/2"
            :block
            {:block-type :LITERAL_BLOCK
             :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
             :content "2"
             :type [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#integer"]}})))
  (testing "expression"
    (is (= (expand-names
            {:prefix-iri {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"}
             :label-iri {"2" "http://foo.com/2"}
             :block
             {:block-type :EXPRESSION_BLOCK
              :predicate [:PREFIXED_NAME "rdfs" ":" "subClassOf"]
              :expression
              [:MN_CLASS_EXPRESSION
               [:MN_NEGATION "not" [:MN_NAME [:MN_LABEL "2"]]]]}})
           {:prefix-iri {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"}
            :label-iri {"2" "http://foo.com/2"}
            :block
            {:block-type :EXPRESSION_BLOCK
             :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#subClassOf"]
             :expression
             [:MN_CLASS_EXPRESSION
              [:MN_NEGATION
               "not"
               [:MN_NAME [:ABSOLUTE_IRI "http://foo.com/2"]]]]}}))))
