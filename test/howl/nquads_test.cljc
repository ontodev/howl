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
             [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]]})))
  (testing "expression label"
    (is (= (convert-quads
            {:current-subject "http://foo.com/foo"
             :block
             {:block-type :EXPRESSION_BLOCK
              :predicate [:ABSOLUTE_IRI (str rdf "type")]
              :expression [:MN_NAME [:ABSOLUTE_IRI (str owl "Class")]]}})
           {:current-subject "http://foo.com/foo"
            :block
            {:block-type :EXPRESSION_BLOCK
             :predicate [:ABSOLUTE_IRI (str rdf "type")]
             :expression [:MN_NAME [:ABSOLUTE_IRI (str owl "Class")]]} 
            :statements
            [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]
            :quads
            [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]}))))


(deftest test-quad-to-string
  (testing "basics"
    (is (= (map
            quad-to-string
            [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]
             [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
             [nil "_:b0" (str rdf "type") (str owl "Axiom")]
             [nil "_:b0" (str owl "annotatedSource")   "http://foo.com/foo"]
             [nil "_:b0" (str owl "annotatedProperty") (str rdf "type")]
             [nil "_:b0" (str owl "annotatedTarget")   (str owl "Class")]
             [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]])
           ["<http://foo.com/foo> <http://www.w3.org/2000/01/rdf-schema#label> \"foo\"@en ."
            "<http://foo.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> ."
            "<_:b0> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> ."
            "<_:b0> <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> ."
            "<_:b0> <http://www.w3.org/2002/07/owl#annotatedProperty> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ."
            "<_:b0> <http://www.w3.org/2002/07/owl#annotatedTarget> <http://www.w3.org/2002/07/owl#Class> ."
            "<_:b0> <http://www.w3.org/2000/01/rdf-schema#seeAlso> <http://www.w3.org/2002/07/owl#Class> ."]))))


