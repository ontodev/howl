(ns howl.expression-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]

            [howl.expression :as exp]
            [howl.core :as core]))

(deftest test-basic-parsing
  (testing "Simple label"
    (is (= (exp/manchester-parser "foo")
           [:CLASS_EXPRESSION [:NAME [:LABEL "foo"]]])))

  (testing "Quoted label"
    (is (= (exp/manchester-parser "'foo'")
           [:CLASS_EXPRESSION [:NAME [:QUOTED_LABEL "'" "foo" "'"]]])))

  (testing "Parens"
    (is (= (exp/manchester-parser "(foo )")
           [:CLASS_EXPRESSION
            "(" [:CLASS_EXPRESSION [:NAME [:LABEL "foo"]]]
            [:SPACE " "] ")"])))

  (testing "Disjunction"
    (is (= (exp/manchester-parser "foo or bar")
           [:CLASS_EXPRESSION
            [:DISJUNCTION
             [:CLASS_EXPRESSION [:NAME [:LABEL "foo"]]]
             [:SPACE " "] "or" [:SPACE " "]
             [:CLASS_EXPRESSION [:NAME [:LABEL "bar"]]]]])))

  (testing "Conjunction"
    (is (= (exp/manchester-parser "foo and bar")
           [:CLASS_EXPRESSION
            [:CONJUNCTION [:CLASS_EXPRESSION [:NAME [:LABEL "foo"]]]
             [:SPACE " "] "and" [:SPACE " "]
             [:CLASS_EXPRESSION [:NAME [:LABEL "bar"]]]]])))

  (testing "Negation"
    (is (= (exp/manchester-parser "not foo")
           [:CLASS_EXPRESSION
            [:NEGATION
             "not" [:SPACE " "]
             [:NAME [:LABEL "foo"]]]])))

  (testing "Some"
    (is (= (exp/manchester-parser "'has part' some foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:NAME [:QUOTED_LABEL "'" "has part" "'"]]]
             [:SPACE " "] "some" [:SPACE " "]
             [:CLASS_EXPRESSION [:NAME [:LABEL "foo"]]]]])))

  (testing "Some not"
    (is (= (exp/manchester-parser "'has part' some not foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:NAME [:QUOTED_LABEL "'" "has part" "'"]]]
             [:SPACE " "] "some" [:SPACE " "]
             [:CLASS_EXPRESSION
              [:NEGATION
               "not" [:SPACE " "]
               [:NAME [:LABEL "foo"]]]]]])))

  (testing "Complex axiom"
    (is (= (exp/manchester-parser "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION [:NAME [:QUOTED_LABEL "'" "is about" "'"]]]
             [:SPACE " "] "some" [:SPACE "\n    "]
             [:CLASS_EXPRESSION
              "(" [:CLASS_EXPRESSION
                   [:CONJUNCTION
                    [:CLASS_EXPRESSION
                     [:NAME [:QUOTED_LABEL
                             "'" "material entity" "'"]]]
                    [:SPACE "\n     "] "and" [:SPACE " "]
                    [:CLASS_EXPRESSION
                     "(" [:CLASS_EXPRESSION
                          [:SOME
                           [:OBJECT_PROPERTY_EXPRESSION
                            [:NAME [:QUOTED_LABEL
                                    "'" "has role" "'"]]]
                           [:SPACE " "] "some" [:SPACE " "]
                           [:CLASS_EXPRESSION
                            [:NAME
                             [:QUOTED_LABEL "'" "evaluant role" "'"]]]]] ")"]]] ")"]]])))

  (testing "Another complex axiom"
    (is (= (exp/manchester-parser
            "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:NAME [:LABEL "has_specified_output"]]]
             [:SPACE " "] "some" [:SPACE "\n"]
             [:CLASS_EXPRESSION
              "(" [:CLASS_EXPRESSION
                   [:CONJUNCTION
                    [:CLASS_EXPRESSION
                     [:NAME [:QUOTED_LABEL "'" "information content entity" "'"]]]
                    [:SPACE "\n "] "and" [:SPACE " "]
                    [:CLASS_EXPRESSION
                     "(" [:CLASS_EXPRESSION
                          [:SOME
                           [:OBJECT_PROPERTY_EXPRESSION
                            [:NAME [:QUOTED_LABEL
                                    "'" "is about" "'"]]]
                           [:SPACE " "] "some" [:SPACE "\n    "]
                           [:CLASS_EXPRESSION
                            "(" [:CLASS_EXPRESSION
                                 [:CONJUNCTION
                                  [:CLASS_EXPRESSION
                                   [:NAME [:QUOTED_LABEL
                                           "'" "material entity" "'"]]]
                                  [:SPACE "\n     "] "and" [:SPACE " "]
                                  [:CLASS_EXPRESSION
                                   "(" [:CLASS_EXPRESSION
                                        [:SOME
                                         [:OBJECT_PROPERTY_EXPRESSION
                                          [:NAME [:QUOTED_LABEL
                                                  "'" "has role" "'"]]]
                                         [:SPACE " "] "some" [:SPACE " "]
                                         [:CLASS_EXPRESSION
                                          [:NAME [:QUOTED_LABEL
                                                  "'" "evaluant role" "'"]]]]] ")"]]] ")"]]] ")"]]] ")"]]]))))

(defn round-trip? [str]
  (is (= str (exp/manchester-format (exp/manchester-parser str)))))

(deftest test-basic-formatting
  (testing "Simple label"
    (round-trip? "foo"))

  (testing "Quoted label"
    (round-trip? "'foo'"))

  (testing "Parens"
    (round-trip? "(foo)"))

  (testing "Parens with space"
    (round-trip? "( foo )"))

  (testing "Disjunction"
    (round-trip? "foo or bar"))

  (testing "Conjunction"
    (round-trip? "foo and bar"))

  (testing "NEGATION"
    (round-trip? "not bar"))

  (testing "Some"
    (round-trip? "'has part' some foo"))

  (testing "Some not"
    (round-trip? "'has part' some not foo"))

  (testing "Complex axiom (preserving newlines)"
    (round-trip? "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))"))

  (testing "Another complex axiom (preserving newlines)"
    (round-trip? "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))")))

;; TODO - test expression->nquads
