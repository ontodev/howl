(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]
            [howl.nquads :refer :all]))

(deftest test-convert
  (testing "literal"
    (is (= (convert-quads
            {:current-subject "http://foo.com/foo"
             :block
             {:block-type :LITERAL_BLOCK
             :predicate [:ABSOLUTE_IRI (str rdfs "label")]
              :value "foo"
              :lang "en"}})
           {:current-subject "http://foo.com/foo"
            :block
            {:block-type :LITERAL_BLOCK
             :predicate [:ABSOLUTE_IRI (str rdfs "label")]
             :value "foo"
             :lang "en"}
            :statements
            [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]
            :quads
            [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]})))
  (testing "link"
    (is (= (convert-quads
            {:current-subject "http://foo.com/foo"
             :block
             {:block-type :LINK_BLOCK
              :predicate [:ABSOLUTE_IRI (str rdf "type")]
              :object [:ABSOLUTE_IRI (str owl "Class")]}})
           {:current-subject "http://foo.com/foo"
            :block
            {:block-type :LINK_BLOCK
             :predicate [:ABSOLUTE_IRI (str rdf "type")]
             :object [:ABSOLUTE_IRI (str owl "Class")]}
            :statements
            [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]
            :quads
            [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]})))
  (testing "nested link annotation"
    (is (= (convert-quads
            {:block
             {:block-type :LINK_BLOCK
              :arrows ">>"
              :predicate [:ABSOLUTE_IRI (str rdfs "seeAlso")]
              :object [:ABSOLUTE_IRI (str owl "Class")]}
             :statements
             ["A"
              [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
              "C"
              "D"
              "E"]})
           {:block
            {:block-type :LINK_BLOCK
             :arrows ">>"
             :predicate [:ABSOLUTE_IRI (str rdfs "seeAlso")]
             :object [:ABSOLUTE_IRI (str owl "Class")]}
            :blank-nodes 1
            :statements
            ["A"
             [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
             [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]]
            :quads
            [[nil "_:b0" (str rdf "type") (str owl "Axiom")]
             [nil "_:b0" (str owl "annotatedSource")   "http://foo.com/foo"]
             [nil "_:b0" (str owl "annotatedProperty") (str rdf "type")]
             [nil "_:b0" (str owl "annotatedTarget")   (str owl "Class")]
             [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]]}))))



(def test-state
  {:base "http://example.com/"
   :prefix-iri {"ex" "http://example.com/"}
   :label-iri {"foo" "http://example.com/foo"}})

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

(deftest test-render-block
  (testing "BASE"
    (let [[state quads]
          (render-block
           {}
           {:block-type :BASE_BLOCK
            :base [:ABSOLUTE_IRI "http://example.com/"]})]
      (is (= state {:base "http://example.com/"}))
      (is (nil? quads))))
  (testing "PREFIX"
    (let [[state quads]
          (render-block
           {}
           {:block-type :PREFIX_BLOCK
            :prefix "ex"
            :prefixed [:ABSOLUTE_IRI "http://example.com/"]})]
      (is (= state {:prefix-iri {"ex" "http://example.com/"}}))
      (is (nil? quads))))
  (testing "LABEL"
    (let [[state quads]
          (render-block
           {}
           {:block-type :LABEL_BLOCK
            :label "foo"
            :identifier [:WRAPPED_IRI "<" "http://example.com/foo" ">"]})]
      (is (= state {:label-iri {"foo" "http://example.com/foo"}}))
      (is (nil? quads))))
  (testing "TYPE"
    (let [[state quads]
          (render-block
           {}
           {:block-type :TYPE_BLOCK
            :predicate [:WRAPPED_IRI "<" "http://example.com/foo" ">"]
            :language "en"})]
      (is (= state {:types-language {"http://example.com/foo" "en"}}))
      (is (nil? quads))))
  (testing "GRAPH"
    (let [[state quads]
          (render-block
           {}
           {:block-type :GRAPH_BLOCK
            :graph [:WRAPPED_IRI "<" "http://example.com/foo" ">"]})]
      (is (= state
             {:graph "<http://example.com/foo>"
              :subjects [["<http://example.com/foo>" "<http://example.com/foo>"]]}))
      (is (nil? quads))))
  (testing "Subject"
    (let [[state quads]
          (render-block
           {}
           {:block-type :SUBJECT_BLOCK
            :subject [:WRAPPED_IRI "<" "http://example.com/foo" ">"]})]
      (is (= state {:subjects [[nil "<http://example.com/foo>"]]}))
      (is (nil? quads))))
  (testing "Literal"
    (let [[state quads]
          (render-block
           {:subjects [[nil "<http://example.com/foo>"]]}
           {:block-type :LITERAL_BLOCK
            :predicate [:WRAPPED_IRI "<" "http://example.com/foo" ">"]
            :content "FOO"})]
      (is (= state
             {:subjects
              [[nil "<http://example.com/foo>" "<http://example.com/foo>" "\"FOO\""]]}))
      (is (= quads
             [[nil "<http://example.com/foo>" "<http://example.com/foo>" "\"FOO\""]]))))
  (testing "Link"
    (let [[state quads]
          (render-block
           {:subjects [[nil "<http://example.com/foo>"]]}
           {:block-type :LINK_BLOCK
            :predicate [:WRAPPED_IRI "<" "http://example.com/foo" ">"]
            :object [:WRAPPED_IRI "<" "http://example.com/bar" ">"]})]
      (is (= state
             {:subjects
              [[nil
                "<http://example.com/foo>"
                "<http://example.com/foo>"
                "<http://example.com/bar>"]]}))
      (is (= quads
             [[nil
               "<http://example.com/foo>"
               "<http://example.com/foo>"
               "<http://example.com/bar>"]]))))
  (testing "Annotation"
    (let [[state quads]
          (render-block
           {:subjects
            [[nil
              "<http://example.com/foo>"
              "<http://example.com/foo>"
              "<http://example.com/bar>"]]}
           {:block-type :LITERAL_BLOCK
            :arrows ">"
            :predicate [:WRAPPED_IRI "<" "http://example.com/foo" ">"]
            :content "FOO"})]
      (is (= state
             {:blank-node-count 1
              :subjects
              [[nil
                "<http://example.com/foo>"
                "<http://example.com/foo>"
                "<http://example.com/bar>"]
               [nil "_:b1" "<http://example.com/foo>" "\"FOO\""]]}))
      (is (= quads
             [[nil
               "_:b1"
               "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               "<http://www.w3.org/2002/07/owl#Axiom>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedSource>"
               "<http://example.com/foo>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedProperty>"
               "<http://example.com/foo>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedTarget>"
               "<http://example.com/bar>"]
              [nil
               "_:b1"
               "<http://example.com/foo>"
               "\"FOO\""]]))))
  ; more
  )
