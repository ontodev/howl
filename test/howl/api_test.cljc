(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [howl.api :refer :all]))

(def test-howl "
# Initial blank line
PREFIX ex:> http://ex.com/
BASE http://foo.com/
LABEL ex:label: label
TYPE ex:label:> @en
<foo>
label: FOO
> label: BAR
GRAPH <baz>
label: BAZ")

(def test-nquads
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:b0 <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> .
_:b0 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://ex.com/label> .
_:b0 <http://www.w3.org/2002/07/owl#annotatedTarget> \"FOO\"@en .
_:b0 <http://ex.com/label> \"BAR\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

(deftest test-howl-to-nquads
  ;; (testing "Render some HOWL to N-Quads"
  ;;   (is (= (howl-to-nquads test-howl)
  ;;          test-nquads)))
  )
