(ns howl.convert-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [edn-ld.jena]
            [howl.core :as core]
            [howl.nquads :as nq]
            [howl.api :as api]))

; Case 1: RDF Quads

(def test-howl-context "
# Initial blank line
PREFIX ex:> http://ex.com/
BASE http://foo.com/
LABEL ex:label: label
LABEL http://www.w3.org/2000/01/rdf-schema#subClassOf: subclass of
TYPE label:> @en
")

(def test-howl "
<foo>
label: FOO

GRAPH <baz>
label: BAZ")

(def test-quad-string
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

(def test-quads
  (second (edn-ld.jena/read-quad-string test-quad-string "n-quads")))

(def test-state
  {:base "http://foo.com/"
   :prefixes {"ex" "http://ex.com/"}
   :labels {"label" "http://ex.com/label"}
   :reverse-labels
   {"http://ex.com/label" "label"
    "http://www.w3.org/2000/01/rdf-schema#subClassOf" "subclass of"
    "http://foo.com/A" "has part"
    "http://foo.com/B" "B"}
   :types-language {"http://ex.com/label" "@en"}})

(defn render-quads
  [state quads]
  (->> quads
       nq/quads-to-howl
       (map (partial core/rename state))
       (map core/render-block)
       (string/join "\n")))

(deftest test-render-howl
  (testing "Render N-Quads to HOWL"
    (is (= (render-quads test-state test-quads)
           test-howl))))


; Case 2: OWL annotations

(def test-howl-2 "
<foo>
label: FOO
> label: BAR
>> label: BAT

GRAPH <baz>
label: BAZ")

(def test-quad-string-2
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
_:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://ex.com/label> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedTarget> \"FOO\"@en .
_:b1 <http://ex.com/label> \"BAR\"@en .
_:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:b2 <http://www.w3.org/2002/07/owl#annotatedSource> _:b1 .
_:b2 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://ex.com/label> .
_:b2 <http://www.w3.org/2002/07/owl#annotatedTarget> \"BAR\"@en .
_:b2 <http://ex.com/label> \"BAT\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

(def test-quads-2
  (second (edn-ld.jena/read-quad-string test-quad-string-2 "n-quads")))

(deftest test-render-howl-2
  (testing "Render N-Quads to HOWL with nested OWL annotations"
    (is (= (render-quads test-state test-quads-2)
           test-howl-2))))


;; Case 3: Manchester

(def test-howl-3 "
<C>
label: C
subclass of:>> 'has part' some (not B)")

(def test-quad-string-3
  "<http://foo.com/C> <http://ex.com/label> \"C\"@en .
<http://foo.com/C> <http://www.w3.org/2000/01/rdf-schema#subClassOf> _:b1 .
_:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> .
_:b1 <http://www.w3.org/2002/07/owl#onProperty> <http://foo.com/A> .
_:b1 <http://www.w3.org/2002/07/owl#someValuesFrom> _:b2 .
_:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> .
_:b2 <http://www.w3.org/2002/07/owl#complementOf> <http://foo.com/B> .")

(def intermediate-1
  {"foo" {"subClassOf" "_:b1"}
   "_:b1" [:owl-some "http://foo.com/part" "_:b2"]
   "_:b2" [:owl-not "http://foo.com/bar"]})

(def intermediate-2
  {"foo" [:owl-some "http://foo.com/part" [:owl-not "http://foo.com/bar"]]})

(def test-quads-3
  (second (edn-ld.jena/read-quad-string test-quad-string-3 "n-quads")))

(def test-subject-map-3 (get (nq/graphify test-quads-3) nq/arq-default-graph))

#_(deftest test-reduce-expression
    (testing "Reduce 'some'"
      (is (= (nq/process-expressions test-subject-map-3)
             nil))))

(deftest test-render-howl-3
  (testing "Render N-Quads to HOWL with OWL logic"
    (is (= (render-quads test-state test-quads-3)
           test-howl-3))))
