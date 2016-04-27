(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [howl.api :refer :all]))

(def test-howl
  "PREFIX ex:> <http://ex.com/>
BASE <http://foo.com/>
# COMMENT
LABEL ex:label: label
TYPE ex:label: @en
<foo>
label: FOO
> label: BAR
GRAPH <baz>
label: BAZ")

(def test-quads
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
_:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://ex.com/label> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedTarget> \"FOO\"@en .
_:b1 <http://ex.com/label> \"BAR\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

(deftest test-render-quads
  (testing "Render some HOWL to N-Quads"
    (is (= (convert-to-quads test-howl)
           test-quads))))

