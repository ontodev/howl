(ns howl.convert-test
  "Integration tests against API."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [edn-ld.jena]
            [howl.core :as core]
            [howl.nquads :as nq]
            [howl.api :as api]))

(def test-howl-context "
# Initial blank line
PREFIX ex:> http://ex.com/
BASE http://foo.com/
LABEL ex:label: label
TYPE label:> @en
")

(def test-howl-content "
<foo>
label: FOO

GRAPH <baz>
label: BAZ")

(def test-howl (str test-howl-context test-howl-content))

(def test-quad-string
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

(def test-quads
  (second (edn-ld.jena/read-quad-string test-quad-string "n-quads")))

(def test-state
  {:base "http://foo.com/"
   :prefixes {"ex" "http://ex.com/"}
   :labels {"label" "http://ex.com/label"}
   :reverse-labels {"http://ex.com/label" "label"}
   :types-language {"http://ex.com/label" "@en"}})

(deftest test-render-howl
  (testing "Render N-Quads to HOWL"
    (is (= (->> test-quads
                (nq/quads-to-howl test-state)
                (map core/render-block)
                (string/join "\n"))
           test-howl-content))))

(def test-howl-content-2
  "<foo>
label: FOO
> label: BAR
>> label: BAT
GRAPH <baz>
label: BAZ")

(def test-quads-2
  "<http://foo.com/foo> <http://ex.com/label> \"FOO\"@en .
_:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://ex.com/label> .
_:b1 <http://www.w3.org/2002/07/owl#annotatedTarget> \"FOO\"@en .
_:b1 <http://ex.com/label> \"BAR\"@en .
<http://foo.com/baz> <http://ex.com/label> \"BAZ\"@en <http://foo.com/baz> .")

