(ns howl.manchester-test
  (:require [clojure.test :refer :all]
            [howl.nquads :refer [rdf owl] :as nq]
            [howl.core :as core]))

(def example-state
  {:label-iri
   {"foo"      "http://foo"
    "bar"      "http://bar"
    "has part" "http://part"}})

(def obo "http://purl.obolibrary.org/obo/")

(def bigger-state
  {:label-iri
   {"has part"                   (str obo "BFO_0000051")
    "has_specified_output"       (str obo "OBI_0000299")
    "information content entity" (str obo "IAO_0000030")
    "is about"                   (str obo "IAO_0000136")
    "material entity"            (str obo "BFO_0000040")
    "has role"                   (str obo "RO_0000087")
    "evaluant role"              (str obo "OBI_0000067")
    "foo"                        "foo"
    "bar"                        "bar"}})

(defn manchester-to-block
  [manchester]
  (->> {:block {:line (str "subclass of:>> " manchester)}}
       core/parse-block
       core/annotate-block
       :block
       :expression))

(defn test-expression
  "Given a Manchester string, a parse vector, and some quads
   test whether the Manchester coonverts to that parse vector,
   and then to those quads."
  [start-state manchester parse result]
  (is (= parse
         (manchester-to-block manchester)))
  (is (= result
         (nq/convert-expression
          {}
          (core/expand-manchester-labels start-state parse)))))

(deftest test-manchester-class-expression
  (testing "Simple label"
    (test-expression
     example-state
     "foo"
     [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
     {:node "http://foo"}))

  (testing "Quoted label"
    (test-expression
     example-state
     "'foo'"
     [:MN_CLASS_EXPRESSION
      [:MN_NAME [:MN_QUOTED_LABEL "'" "foo" "'"]]]
     {:node "http://foo"}))

  (testing "Parens"
    (test-expression
     example-state
     "(foo )"
     [:MN_CLASS_EXPRESSION
      "("
      [:MN_CLASS_EXPRESSION
       [:MN_NAME [:MN_LABEL "foo"]]] [:MN_SPACE " "]
      ")"]
     {:node "http://foo"}))

  (testing "Negation"
    (test-expression
     example-state
     "not foo"
     [:MN_CLASS_EXPRESSION
      [:MN_NEGATION
       "not"
       [:MN_SPACE " "]
       [:MN_NAME [:MN_LABEL "foo"]]]]
     {:blank-node-count 1
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Class")]
       [nil "_:b1" (str owl "complementOf") "http://foo"]]}))

  (testing "Disjunction"
    (test-expression
     example-state
     "foo or bar"
     [:MN_CLASS_EXPRESSION
      [:MN_DISJUNCTION
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
       [:MN_SPACE " "]
       "or"
       [:MN_SPACE " "]
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "bar"]]]]]
     {:blank-node-count 3
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Class")]
       [nil "_:b1" (str owl "unionOf") "_:b2"]
       [nil "_:b2" (str rdf "first") "http://foo"]
       [nil "_:b2" (str rdf "rest") "_:b3"]
       [nil "_:b3" (str rdf "first") "http://bar"]
       [nil "_:b3" (str rdf "rest") (str rdf "nil")]]}))

  (testing "Conjunction"
    (test-expression
     example-state
     "foo and bar"
     [:MN_CLASS_EXPRESSION
      [:MN_CONJUNCTION
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
       [:MN_SPACE " "]
       "and"
       [:MN_SPACE " "]
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "bar"]]]]]
     {:blank-node-count 3
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Class")]
       [nil "_:b1" (str owl "intersectionOf") "_:b2"]
       [nil "_:b2" (str rdf "first") "http://foo"]
       [nil "_:b2" (str rdf "rest") "_:b3"]
       [nil "_:b3" (str rdf "first") "http://bar"]
       [nil "_:b3" (str rdf "rest") (str rdf "nil")]]}))

  (testing "Some"
    (test-expression
     example-state
     "'has part' some foo"
     [:MN_CLASS_EXPRESSION
      [:MN_SOME
       [:MN_OBJECT_PROPERTY_EXPRESSION
        [:MN_NAME [:MN_QUOTED_LABEL "'" "has part" "'"]]]
       [:MN_SPACE " "]
       "some"
       [:MN_SPACE " "]
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]]]
     {:blank-node-count 1
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Restriction")]
       [nil "_:b1" (str owl "onProperty") "http://part"]
       [nil "_:b1" (str owl "someValuesFrom") "http://foo"]]}))

  (testing "Some Not"
    (test-expression
     example-state
     "'has part' some not foo"
     [:MN_CLASS_EXPRESSION
      [:MN_SOME
       [:MN_OBJECT_PROPERTY_EXPRESSION
        [:MN_NAME [:MN_QUOTED_LABEL "'" "has part" "'"]]]
       [:MN_SPACE " "]
       "some"
       [:MN_SPACE " "]
       [:MN_CLASS_EXPRESSION
        [:MN_NEGATION
         "not"
         [:MN_SPACE " "]
         [:MN_NAME [:MN_LABEL "foo"]]]]]]
     {:blank-node-count 2
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Restriction")]
       [nil "_:b1" (str owl "onProperty") "http://part"]
       [nil "_:b1" (str owl "someValuesFrom") "_:b2"]
       [nil "_:b2" (str rdf "type") (str owl "Class")]
       [nil "_:b2" (str owl "complementOf") "http://foo"]]}))

  (testing "And not"
    (test-expression
     example-state
     "foo and not bar"
     [:MN_CLASS_EXPRESSION
      [:MN_CONJUNCTION
       [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
       [:MN_SPACE " "]
       "and"
       [:MN_SPACE " "]
       [:MN_CLASS_EXPRESSION
        [:MN_NEGATION
         "not"
         [:MN_SPACE " "]
         [:MN_NAME [:MN_LABEL "bar"]]]]]]
     {:blank-node-count 4
      :node "_:b1"
      :quads
      [[nil "_:b1" (str rdf "type") (str owl "Class")]
       [nil "_:b1" (str owl "intersectionOf") "_:b2"]
       [nil "_:b2" (str rdf "first") "http://foo"]
       [nil "_:b2" (str rdf "rest") "_:b3"]
       [nil "_:b3" (str rdf "first") "_:b4"]
       [nil "_:b3" (str rdf "rest") (str rdf "nil")]
       [nil "_:b4" (str rdf "type") (str owl "Class")]
       [nil "_:b4" (str owl "complementOf") "http://bar"]]}))

  (testing "Complex axiom"
    (test-expression
     bigger-state
     "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))"
     [:MN_CLASS_EXPRESSION
      [:MN_SOME
       [:MN_OBJECT_PROPERTY_EXPRESSION
        [:MN_NAME [:MN_QUOTED_LABEL "'" "is about" "'"]]]
       [:MN_SPACE " "]
       "some"
       [:MN_SPACE "\n    "]
       [:MN_CLASS_EXPRESSION
        "("
        [:MN_CLASS_EXPRESSION
         [:MN_CONJUNCTION
          [:MN_CLASS_EXPRESSION
           [:MN_NAME [:MN_QUOTED_LABEL "'" "material entity" "'"]]]
          [:MN_SPACE "\n     "]
          "and"
          [:MN_SPACE " "]
          [:MN_CLASS_EXPRESSION
           "("
           [:MN_CLASS_EXPRESSION
            [:MN_SOME
             [:MN_OBJECT_PROPERTY_EXPRESSION
              [:MN_NAME [:MN_QUOTED_LABEL "'" "has role" "'"]]]
             [:MN_SPACE " "]
             "some"
             [:MN_SPACE " "]
             [:MN_CLASS_EXPRESSION
              [:MN_NAME [:MN_QUOTED_LABEL "'" "evaluant role" "'"]]]]]
           ")"]]]
        ")"]]]
     {:blank-node-count 5
      :node "_:b1"
      :quads
      [; 'is about' some X
       [nil "_:b1" (str rdf "type") (str owl "Restriction")]
       [nil "_:b1" (str owl "onProperty") (str obo "IAO_0000136")]
       [nil "_:b1" (str owl "someValuesFrom") "_:b2"]

       ; 'material entity' and X
       [nil "_:b2" (str rdf "type") (str owl "Class")]
       [nil "_:b2" (str owl "intersectionOf") "_:b3"]
       [nil "_:b3" (str rdf "first") (str obo "BFO_0000040")]
       [nil "_:b3" (str rdf "rest") "_:b4"]
       [nil "_:b4" (str rdf "first") "_:b5"]
       [nil "_:b4" (str rdf "rest") (str rdf "nil")]

       ; 'has role' some 'evaluant role'
       [nil "_:b5" (str rdf "type") (str owl "Restriction")]
       [nil "_:b5" (str owl "onProperty") (str obo "RO_0000087")]
       [nil "_:b5" (str owl "someValuesFrom") (str obo "OBI_0000067")]]}))

  (testing "More complex axiom"
    (test-expression
     bigger-state
          "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))"
     [:MN_CLASS_EXPRESSION
      [:MN_SOME
       [:MN_OBJECT_PROPERTY_EXPRESSION
        [:MN_NAME [:MN_LABEL "has_specified_output"]]]
       [:MN_SPACE " "]
       "some"
       [:MN_SPACE "\n"]
       [:MN_CLASS_EXPRESSION
        "("
        [:MN_CLASS_EXPRESSION
         [:MN_CONJUNCTION
          [:MN_CLASS_EXPRESSION
           [:MN_NAME [:MN_QUOTED_LABEL "'" "information content entity" "'"]]]
          [:MN_SPACE "\n "]
          "and"
          [:MN_SPACE " "]
          [:MN_CLASS_EXPRESSION
           "("
           [:MN_CLASS_EXPRESSION
            [:MN_SOME
             [:MN_OBJECT_PROPERTY_EXPRESSION
              [:MN_NAME [:MN_QUOTED_LABEL "'" "is about" "'"]]]
             [:MN_SPACE " "]
             "some"
             [:MN_SPACE "\n    "]
             [:MN_CLASS_EXPRESSION
              "("
              [:MN_CLASS_EXPRESSION
               [:MN_CONJUNCTION
                [:MN_CLASS_EXPRESSION
                 [:MN_NAME [:MN_QUOTED_LABEL "'" "material entity" "'"]]]
                [:MN_SPACE "\n     "]
                "and"
                [:MN_SPACE " "]
                [:MN_CLASS_EXPRESSION
                 "("
                 [:MN_CLASS_EXPRESSION
                  [:MN_SOME
                   [:MN_OBJECT_PROPERTY_EXPRESSION
                    [:MN_NAME [:MN_QUOTED_LABEL "'" "has role" "'"]]]
                   [:MN_SPACE " "]
                   "some"
                   [:MN_SPACE " "]
                   [:MN_CLASS_EXPRESSION
                    [:MN_NAME [:MN_QUOTED_LABEL "'" "evaluant role" "'"]]]]]
                 ")"]]]
              ")"]]]
           ")"]]]
        ")"]]]
     {:blank-node-count 9
      :node "_:b1"
      :quads
      [; has_specified_output somme X
       [nil "_:b1" (str rdf "type") (str owl "Restriction")]
       [nil "_:b1" (str owl "onProperty") (str obo "OBI_0000299")]
       [nil "_:b1" (str owl "someValuesFrom") "_:b2"]

       ; ('information content entity' and X)
       [nil "_:b2" (str rdf "type") (str owl "Class")]
       [nil "_:b2" (str owl "intersectionOf") "_:b3"]
       [nil "_:b3" (str rdf "first") (str obo "IAO_0000030")]
       [nil "_:b3" (str rdf "rest") "_:b4"]
       [nil "_:b4" (str rdf "first") "_:b5"]
       [nil "_:b4" (str rdf "rest") (str rdf "nil")]

       ; 'is about' some X
       [nil "_:b5" (str rdf "type") (str owl "Restriction")]
       [nil "_:b5" (str owl "onProperty") (str obo "IAO_0000136")]
       [nil "_:b5" (str owl "someValuesFrom") "_:b6"]

       ; 'material entity' and X
       [nil "_:b6" (str rdf "type") (str owl "Class")]
       [nil "_:b6" (str owl "intersectionOf") "_:b7"]
       [nil "_:b7" (str rdf "first") (str obo "BFO_0000040")]
       [nil "_:b7" (str rdf "rest") "_:b8"]
       [nil "_:b8" (str rdf "first") "_:b9"]
       [nil "_:b8" (str rdf "rest") (str rdf "nil")]

       ; 'has role' some 'evaluant role'
       [nil "_:b9" (str rdf "type") (str owl "Restriction")]
       [nil "_:b9" (str owl "onProperty") (str obo "RO_0000087")]
       [nil "_:b9" (str owl "someValuesFrom") (str obo "OBI_0000067")]]}))
  ; more tests?
  )
