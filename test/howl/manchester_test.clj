(ns howl.manchester-test
  (:require [clojure.test :refer :all]
            [howl.nquads :refer [render-expression iri rdf owl]]
            [howl.core :refer [block-parser]]))

(def example-state
  {:labels
   {"foo"      "foo"
    "bar"      "bar"
    "has part" "part"}})

(def obo "http://purl.obolibrary.org/obo/")

(def bigger-state
  {:labels
   {"has part"                   (str obo "BFO_0000051")
    "has_specified_output"       (str obo "OBI_0000299")
    "information content entity" (str obo "IAO_0000030")
    "is about"                   (str obo "IAO_0000136")
    "material entity"            (str obo "BFO_0000040")
    "has role"                   (str obo "RO_0000087")
    "evaluant role"              (str obo "OBI_0000067")
    "foo"                        "foo"
    "bar"                        "bar"}})

(defn ce-parser
  [expression]
  (get-in (block-parser (str "subclass of:>> " expression)) [1 3]))

(deftest test-manchester-class-expression
  (testing "Simple label"
    (let [mn-string "foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Quoted label"
    (let [mn-string "'foo'"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              [:MN_NAME [:MN_QUOTED_LABEL "'" "foo" "'"]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Parens"
    (let [mn-string "(foo )"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              "("
              [:MN_CLASS_EXPRESSION
               [:MN_NAME [:MN_LABEL "foo"]]] [:MN_SPACE " "]
              ")"]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:node "<foo>"})))))

  (testing "Negation"
    (let [mn-string "not foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              [:MN_NEGATION
               "not"
               [:MN_SPACE " "]
               [:MN_NAME [:MN_LABEL "foo"]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 1
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Class")]
                [nil "_:b1" (iri owl "complementOf") "<foo>"]]})))))

  (testing "Disjunction"
    (let [mn-string "foo or bar"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              [:MN_DISJUNCTION
               [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
               [:MN_SPACE " "]
               "or"
               [:MN_SPACE " "]
               [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "bar"]]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 3
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Class")]
                [nil "_:b1" (iri owl "unionOf") "_:b2"]
                [nil "_:b2" (iri rdf "first") "<foo>"]
                [nil "_:b2" (iri rdf "rest") "_:b3"]
                [nil "_:b3" (iri rdf "first") "<bar>"]
                [nil "_:b3" (iri rdf "rest") (iri rdf "nil")]]})))))

  (testing "Conjunction"
    (let [mn-string "foo and bar"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              [:MN_CONJUNCTION
               [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]
               [:MN_SPACE " "]
               "and"
               [:MN_SPACE " "]
               [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "bar"]]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 3
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Class")]
                [nil "_:b1" (iri owl "intersectionOf") "_:b2"]
                [nil "_:b2" (iri rdf "first") "<foo>"]
                [nil "_:b2" (iri rdf "rest") "_:b3"]
                [nil "_:b3" (iri rdf "first") "<bar>"]
                [nil "_:b3" (iri rdf "rest") (iri rdf "nil")]]})))))

  (testing "Some"
    (let [mn-string "'has part' some foo"
          parse     (ce-parser mn-string)]
      (is (= parse
             [:MN_CLASS_EXPRESSION
              [:MN_SOME
               [:MN_OBJECT_PROPERTY_EXPRESSION
                [:MN_NAME [:MN_QUOTED_LABEL "'" "has part" "'"]]]
               [:MN_SPACE " "]
               "some"
               [:MN_SPACE " "]
               [:MN_CLASS_EXPRESSION [:MN_NAME [:MN_LABEL "foo"]]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 1
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b1" (iri owl "onProperty") "<part>"]
                [nil "_:b1" (iri owl "someValuesFrom") "<foo>"]]})))))

  (testing "Some Not"
    (let [mn-string "'has part' some not foo"
          parse     (ce-parser mn-string)]
      (is (= parse
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
                 [:MN_NAME [:MN_LABEL "foo"]]]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 2
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b1" (iri owl "onProperty") "<part>"]
                [nil "_:b1" (iri owl "someValuesFrom") "_:b2"]
                [nil "_:b2" (iri rdf "type") (iri owl "Class")]
                [nil "_:b2" (iri owl "complementOf") "<foo>"]]})))))

  (testing "And not"
    (let [mn-string "foo and not bar"
          parse     (ce-parser mn-string)]
      (is (= parse
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
                 [:MN_NAME [:MN_LABEL "bar"]]]]]]))
      (is (= (render-expression example-state {} parse)
             (merge
              example-state
              {:blank-node-count 4
               :node "_:b1"
               :quads
               [[nil "_:b1" (iri rdf "type") (iri owl "Class")]
                [nil "_:b1" (iri owl "intersectionOf") "_:b2"]
                [nil "_:b2" (iri rdf "first") "<foo>"]
                [nil "_:b2" (iri rdf "rest") "_:b3"]
                [nil "_:b3" (iri rdf "first") "_:b4"]
                [nil "_:b3" (iri rdf "rest") (iri rdf "nil")]
                [nil "_:b4" (iri rdf "type") (iri owl "Class")]
                [nil "_:b4" (iri owl "complementOf") "<bar>"]]})))))

  (testing "Complex axiom"
    (let [mn-string
          "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))"
          parse     (ce-parser mn-string)]
      (is (= parse
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
                ")"]]]))
      (is (= (render-expression bigger-state {} parse)
             (merge
              bigger-state
              {:blank-node-count 5
               :node "_:b1"
               :quads
               [; 'is about' some X
                [nil "_:b1" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b1" (iri owl "onProperty") (iri obo "IAO_0000136")]
                [nil "_:b1" (iri owl "someValuesFrom") "_:b2"]

                ; 'material entity' and X
                [nil "_:b2" (iri rdf "type") (iri owl "Class")]
                [nil "_:b2" (iri owl "intersectionOf") "_:b3"]
                [nil "_:b3" (iri rdf "first") (iri obo "BFO_0000040")]
                [nil "_:b3" (iri rdf "rest") "_:b4"]
                [nil "_:b4" (iri rdf "first") "_:b5"]
                [nil "_:b4" (iri rdf "rest") (iri rdf "nil")]

                ; 'has role' some 'evaluant role'
                [nil "_:b5" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b5" (iri owl "onProperty") (iri obo "RO_0000087")]
                [nil "_:b5" (iri owl "someValuesFrom") (iri obo "OBI_0000067")]]})))))

  (testing "More complex axiom"
    (let [mn-string
          "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))"
          parse     (ce-parser mn-string)]
      (is (= parse
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
                ")"]]]))
      (is (= (render-expression bigger-state {} parse)
             (merge
              bigger-state
              {:blank-node-count 9
               :node "_:b1"
               :quads
               [; has_specified_output somme X
                [nil "_:b1" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b1" (iri owl "onProperty") (iri obo "OBI_0000299")]
                [nil "_:b1" (iri owl "someValuesFrom") "_:b2"]

                ; ('information content entity' and X)
                [nil "_:b2" (iri rdf "type") (iri owl "Class")]
                [nil "_:b2" (iri owl "intersectionOf") "_:b3"]
                [nil "_:b3" (iri rdf "first") (iri obo "IAO_0000030")]
                [nil "_:b3" (iri rdf "rest") "_:b4"]
                [nil "_:b4" (iri rdf "first") "_:b5"]
                [nil "_:b4" (iri rdf "rest") (iri rdf "nil")]
                
                ; 'is about' some X
                [nil "_:b5" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b5" (iri owl "onProperty") (iri obo "IAO_0000136")]
                [nil "_:b5" (iri owl "someValuesFrom") "_:b6"]

                ; 'material entity' and X
                [nil "_:b6" (iri rdf "type") (iri owl "Class")]
                [nil "_:b6" (iri owl "intersectionOf") "_:b7"]
                [nil "_:b7" (iri rdf "first") (iri obo "BFO_0000040")]
                [nil "_:b7" (iri rdf "rest") "_:b8"]
                [nil "_:b8" (iri rdf "first") "_:b9"]
                [nil "_:b8" (iri rdf "rest") (iri rdf "nil")]

                ; 'has role' some 'evaluant role'
                [nil "_:b9" (iri rdf "type") (iri owl "Restriction")]
                [nil "_:b9" (iri owl "onProperty") (iri obo "RO_0000087")]
                [nil "_:b9" (iri owl "someValuesFrom") (iri obo "OBI_0000067")]]})))))
  ; more tests?
  )
