(ns howl.core-test
  (:require [clojure.test :refer :all]
            [howl.core :refer :all]))

(def test-labels
  {:labels
   {"example" "http://example.com/example"}})

(deftest test-resolve-label
  (testing "Basic cases"
    (are
     [label iri]
     (= (resolve-label test-labels label) iri)
      "example" "http://example.com/example"
      "foo" nil)))

(def test-prefixes
  {:prefixes
   {"ex" "http://example.com/"}})

(deftest test-resolve-prefixed-name
  (testing "Basic cases"
    (are
     [prefixed-name iri]
     (= (resolve-prefixed-name test-prefixes prefixed-name) iri)
      "ex:foo" "http://example.com/foo"
      "foo:ex" nil)))

(def test-base-iri
  {:base-iri "http://example.com/"})

(deftest test-resolve-relative-iri
  (testing "Absolute IRI"
    (are
     [relative absolute]
     (= (resolve-relative-iri test-base-iri relative) absolute)
      "foo" "http://example.com/foo"
      "/foo" "http://example.com/foo"
      ; TODO: "ex:foo" "http://example.com/foo"
      "http://example.com/foo" "http://example.com/foo")))

(def test-state
  (merge test-labels test-prefixes test-base-iri))

(deftest test-resolve-name
  (testing "Basic case"
    (are [name iri]
         (= (resolve-name test-state name) iri)
      "example" "http://example.com/example"
      "ex:foo" "http://example.com/foo"
      "foo" "http://example.com/foo"
      "/foo" "http://example.com/foo")))


(def example-state
  {:labels
   {"foo"      "<foo>"
    "bar"      "<bar>"
    "has part" "<part>"}})


(deftest test-rdf-list
  (testing "Three elements"
    (is (= (rdf-list {:blank-node-count 10} ["A" "B" "C"])
           {:blank-node-count 13
            :node "_:b11"
            :triples
            [["_:b11" "rdf:first" "A"]
             ["_:b11" "rdf:rest"  "_:b12"]
             ["_:b12" "rdf:first" "B"]
             ["_:b12" "rdf:rest"  "_:b13"]
             ["_:b13" "rdf:first" "C"]
             ["_:b13" "rdf:rest"  "rdf:nil"]]}))))

(deftest test-manchester-class-expression
  (testing "Simple label"
    (let [mn-string "foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE [:MN_NAME [:LABEL "foo"]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Quoted label"
    (let [mn-string "'foo'"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE [:MN_NAME [:QUOTED_LABEL "'" "foo" "'"]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Parens"
    (let [mn-string "(foo )"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE "(" [:MN_CE [:MN_NAME [:LABEL "foo"]]] [:SPACES " "] ")"]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Negation"
    (let [mn-string "not foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_NEGATION
               "not"
               [:SPACES " "]
               [:MN_NAME [:LABEL "foo"]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 1
               :node "_:b1"
               :triples
               [["_:b1" "rdf:type" "owl:Class"]
                ["_:b1" "owl:complementOf" "<foo>"]]})))))

  (testing "Disjunction"
    (let [mn-string "foo or bar"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_DISJUNCTION
               [:MN_CE [:MN_NAME [:LABEL "foo"]]]
               [:SPACES " "]
               "or"
               [:SPACES " "]
               [:MN_CE [:MN_NAME [:LABEL "bar"]]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 3
               :node "_:b3"
               :triples
               [["_:b1" "rdf:first" "<foo>"]
                ["_:b1" "rdf:rest" "_:b2"]
                ["_:b2" "rdf:first" "<bar>"]
                ["_:b2" "rdf:rest" "rdf:nil"]
                ["_:b3" "rdf:type" "owl:Class"]
                ["_:b3" "owl:unionOf" "_:b1"]]})))))

  (testing "Conjunction"
    (let [mn-string "foo and bar"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_CONJUNCTION
               [:MN_CE [:MN_NAME [:LABEL "foo"]]]
               [:SPACES " "]
               "and"
               [:SPACES " "]
               [:MN_CE [:MN_NAME [:LABEL "bar"]]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 3
               :node "_:b3"
               :triples
               [["_:b1" "rdf:first" "<foo>"]
                ["_:b1" "rdf:rest" "_:b2"]
                ["_:b2" "rdf:first" "<bar>"]
                ["_:b2" "rdf:rest" "rdf:nil"]
                ["_:b3" "rdf:type" "owl:Class"]
                ["_:b3" "owl:intersectionOf" "_:b1"]]})))))

  (testing "Some"
    (let [mn-string "'has part' some foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_SOME
               [:MN_OPE [:MN_NAME [:QUOTED_LABEL "'" "has part" "'"]]]
               [:SPACES " "]
               "some"
               [:SPACES " "]
               [:MN_CE [:MN_NAME [:LABEL "foo"]]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 1
               :node "_:b1"
               :triples
               [["_:b1" "rdf:type" "owl:Restriction"]
                ["_:b1" "owl:onProperty" "<part>"]
                ["_:b1" "owl:someValuesFrom" "<foo>"]]})))))

  (testing "Some Not"
    (let [mn-string "'has part' some not foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_SOME
               [:MN_OPE [:MN_NAME [:QUOTED_LABEL "'" "has part" "'"]]]
               [:SPACES " "]
               "some"
               [:SPACES " "]
               [:MN_CE
                [:MN_NEGATION
                 "not"
                 [:SPACES " "]
                 [:MN_NAME [:LABEL "foo"]]]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 2
               :node "_:b2"
               :triples
               [["_:b1" "rdf:type" "owl:Class"]
                ["_:b1" "owl:complementOf" "<foo>"]
                ["_:b2" "rdf:type" "owl:Restriction"]
                ["_:b2" "owl:onProperty" "<part>"]
                ["_:b2" "owl:someValuesFrom" "_:b1"]]})))))

  (testing "And not"
    (let [mn-string "foo and not bar"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CE
              [:MN_CONJUNCTION
               [:MN_CE [:MN_NAME [:LABEL "foo"]]]
               [:SPACES " "]
               "and"
               [:SPACES " "]
               [:MN_CE
                [:MN_NEGATION
                 "not"
                 [:SPACES " "]
                 [:MN_NAME [:LABEL "bar"]]]]]]))
      (is (= (ce-triples example-state {} parse)
             (merge
              example-state
              {:blank-node-count 4
               :node "_:b4"
               :triples
               [["_:b1" "rdf:type" "owl:Class"]
                ["_:b1" "owl:complementOf" "<bar>"]
                ["_:b2" "rdf:first" "<foo>"]
                ["_:b2" "rdf:rest" "_:b3"]
                ["_:b3" "rdf:first" "_:b1"]
                ["_:b3" "rdf:rest" "rdf:nil"]
                ["_:b4" "rdf:type" "owl:Class"]
                ["_:b4" "owl:intersectionOf" "_:b2"]]})))))

  (testing "Complex axiom"
    (is (= (ce-parser
            "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))")
           [:MN_CE
            [:MN_SOME
             [:MN_OPE [:MN_NAME [:LABEL "has_specified_output"]]]
             [:SPACES " "]
             "some"
             [:SPACES "\n"]
             [:MN_CE
              "("
              [:MN_CE
               [:MN_CONJUNCTION
                [:MN_CE [:MN_NAME [:QUOTED_LABEL "'" "information content entity" "'"]]]
                [:SPACES "\n "]
                "and"
                [:SPACES " "]
                [:MN_CE
                 "("
                 [:MN_CE
                  [:MN_SOME
                   [:MN_OPE [:MN_NAME [:QUOTED_LABEL "'" "is about" "'"]]]
                   [:SPACES " "]
                   "some"
                   [:SPACES "\n    "]
                   [:MN_CE
                    "("
                    [:MN_CE
                     [:MN_CONJUNCTION
                      [:MN_CE [:MN_NAME [:QUOTED_LABEL "'" "material entity" "'"]]]
                      [:SPACES "\n     "]
                      "and"
                      [:SPACES " "]
                      [:MN_CE
                       "("
                       [:MN_CE
                        [:MN_SOME
                         [:MN_OPE [:MN_NAME [:QUOTED_LABEL "'" "has role" "'"]]]
                         [:SPACES " "]
                         "some"
                         [:SPACES " "]
                         [:MN_CE [:MN_NAME [:QUOTED_LABEL "'" "evaluant role" "'"]]]]]
                       ")"]]]
                    ")"]]]
                 ")"]]]
              ")"]]])))
    ; more
  )


"has_specified_output some
    ('information content entity'
     and ('is about' some
        ('material entity'
         and ('has role' some 'evaluant role'))))"

;(deftest test-parse-prefix
;  (testing "Basic case"
;    (is (= (unit-parser
;            "PREFIX rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#")
;           [:PREFIX]))))

;(deftest test-add-prefix
;  (testing "Basic case"
;    (is (= (add-prefix {} 1 "PREFIX foo: <http://example.com/>")
;           {:prefixes
;            {"foo" "http://example.com/"}}))))
;
;(deftest test-add-label
;  (testing "Basic case"
;    (is (= (add-label test-prefixes 1 "LABEL ex:foo: Foo")
;           (merge
;            test-prefixes
;            {:labels
;             {"Foo" "http://example.com/foo"}})))))
;
;(def lines
;  "not indented
;not indented
;  indented
;not indented
;  indented
;
;  after blank
;  indented last")
;
;(deftest test-collect-lines
;  (testing "Basic case"))

;; more
