(ns howl.convert-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [edn-ld.jena]
            [howl.core :as core]
            [howl.nquads :as nq]
            [howl.api :as api]))

(def trig-header "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w4.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://ex.com/> .
@base <http://foo.com/> .

")

(def test-state
  {:base "http://foo.com/"
   :prefix-iri
   {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
    "xsd" "http://www.w4.org/2001/XMLSchema#"
    "owl" "http://www.w3.org/2002/07/owl#"
    "ex" "http://ex.com/"}
   :iri-label
   {"http://www.w3.org/2000/01/rdf-schema#label" "label"
    "http://www.w3.org/2000/01/rdf-schema#subClassOf" "subclass of"
    "http://foo.com/A" "has part"
    "http://foo.com/B" "B"}
   :types-language {"http://ex.com/label" "@en"}})

(defn trig-to-howl
  "Given a TriG string and a HOWL state,
   convert the TriG to HOWL
   and return the HOWL string."
  [trig state]
  (->> (edn-ld.jena/read-quad-string (str trig-header trig) "trig")
       second
       nq/quads-to-howl
       (core/render-howl state)))

(defn test-trig-equals-howl
  "Given a TriG string and a HOWL string,
   test whether the TriG convers to that HOWL with that state."
  [trig blocks howl]
  (let [quads  (second (edn-ld.jena/read-quad-string (str trig-header trig) "trig"))
        bs     (nq/quads-to-howl quads)
        result (core/render-howl test-state bs)]
    (is (= bs blocks))
    (is (= result howl))))

(deftest test-rdf-to-howl
  (testing "Convert statement"
    (test-trig-equals-howl
     "<foo> rdfs:label \"FOO\"@en ."
     [{:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/foo"]
       :label "FOO"}
      {:block-type :SUBJECT_BLOCK
       :subject [:ABSOLUTE_IRI "http://foo.com/foo"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "FOO"
       :lang "en"
       :eol "\n"}]
     "LABEL http://foo.com/foo: FOO

<foo>
label: FOO@en
"))
  (testing "Convert named graph"
    (test-trig-equals-howl
     "<foo> rdfs:label \"FOO\"@en .
      <baz> { <baz> rdfs:label \"BAZ\"@en }"
     [{:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/foo"]
       :label "FOO"}
      {:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/baz"]
       :label "BAZ"}
      {:block-type :SUBJECT_BLOCK
       :subject [:ABSOLUTE_IRI "http://foo.com/foo"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "FOO"
       :lang "en"
       :eol "\n"}
      {:block-type :GRAPH_BLOCK
       :graph [:ABSOLUTE_IRI "http://foo.com/baz"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAZ"
       :lang "en"
       :eol "\n"}]
     "LABEL http://foo.com/foo: FOO
LABEL http://foo.com/baz: BAZ

<foo>
label: FOO@en

GRAPH <baz>
label: BAZ@en
"))
  (testing "multiple OWL annotations"
    (test-trig-equals-howl
     "<foo> rdfs:label \"FOO\"@en .
_:b1 rdf:type owl:Axiom ;
  owl:annotatedSource <foo> ;
  owl:annotatedProperty rdfs:label ;
  owl:annotatedTarget \"FOO\"@en ;
  rdfs:label \"BAR\"@en ;
  rdfs:label \"BAT\"@en .
<baz> { <baz> rdfs:label \"BAZ\"@en }"
     [{:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/foo"]
       :label "FOO"}
      {:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/baz"]
       :label "BAZ"}
      {:block-type :SUBJECT_BLOCK
       :subject [:ABSOLUTE_IRI "http://foo.com/foo"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "FOO"
       :lang "en"
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows "> "
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAR"
       :lang "en"
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows "> "
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAT"
       :lang "en"
       :eol "\n"}
      {:block-type :GRAPH_BLOCK
       :graph [:ABSOLUTE_IRI "http://foo.com/baz"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAZ"
       :lang "en"
       :eol "\n"}]
     "LABEL http://foo.com/foo: FOO
LABEL http://foo.com/baz: BAZ

<foo>
label: FOO@en
> label: BAR@en
> label: BAT@en

GRAPH <baz>
label: BAZ@en
"))
  (testing "nested OWL annotations"
    (test-trig-equals-howl
     "<foo> rdfs:label \"FOO\"@en .
_:b1 rdf:type owl:Axiom ;
  owl:annotatedSource <foo> ;
  owl:annotatedProperty rdfs:label ;
  owl:annotatedTarget \"FOO\"@en ;
  rdfs:label \"BAR\"@en .
_:b2 rdf:type owl:Axiom ;
  owl:annotatedSource _:b1 ;
  owl:annotatedProperty rdfs:label ;
  owl:annotatedTarget \"BAR\"@en ;
  rdfs:label \"BAT\"@en .
<baz> { <baz> rdfs:label \"BAZ\"@en }"
     [{:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/foo"]
       :label "FOO"}
      {:block-type :LABEL_BLOCK
       :eol "\n"
       :identifier [:ABSOLUTE_IRI "http://foo.com/baz"]
       :label "BAZ"}
      {:block-type :SUBJECT_BLOCK
       :subject [:ABSOLUTE_IRI "http://foo.com/foo"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "FOO"
       :lang "en"
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows "> "
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAR"
       :lang "en"
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ">> "
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAT"
       :lang "en"
       :eol "\n"}
      {:block-type :GRAPH_BLOCK
       :graph [:ABSOLUTE_IRI "http://foo.com/baz"]
       :eol "\n"}
      {:block-type :LITERAL_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#label"]
       :value "BAZ"
       :lang "en"
       :eol "\n"}]
     "LABEL http://foo.com/foo: FOO
LABEL http://foo.com/baz: BAZ

<foo>
label: FOO@en
> label: BAR@en
>> label: BAT@en

GRAPH <baz>
label: BAZ@en
"))
  (testing "Manchester"
    (test-trig-equals-howl
     "<C> rdfs:subClassOf _:b1 .
_:b1 rdf:type owl:Restriction ;
  owl:onProperty <A> ;
  owl:someValuesFrom _:b2 .
_:b2 rdf:type owl:Class ;
  owl:complementOf <B> ."
     [{:block-type :SUBJECT_BLOCK
       :subject [:ABSOLUTE_IRI "http://foo.com/C"]
       :eol "\n"}
      {:block-type :EXPRESSION_BLOCK
       :arrows ""
       :predicate [:ABSOLUTE_IRI "http://www.w3.org/2000/01/rdf-schema#subClassOf"]
       :expression
       [:CLASS_EXPRESSION
        "("
        [:SOME
         [:OBJECT_PROPERTY_EXPRESSION "http://foo.com/A"]
         [:SPACE " "]
         "some"
         [:SPACE " "]
         [:CLASS_EXPRESSION
          "("
          [:NEGATION
           "not"
           [:SPACE " "]
           "http://foo.com/B"]
          ")"]]
        ")"]
       :eol "\n"}]
     "<C>
subclass of:>> 'has part' some (not B)
")))
