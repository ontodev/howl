(ns howl.manchester-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [clojure.string :as string]
            [howl.manchester :as man]))

(deftest test-basic-parsing
  (testing "Simple label"
    (is (= (man/parse-manchester "foo")
           [:CLASS_EXPRESSION [:LABEL "" "foo" ""]])))

  (testing "Quoted label"
    (is (= (man/parse-manchester "'foo'")
           [:CLASS_EXPRESSION [:LABEL "'" "foo" "'"]])))

  (testing "Parens"
    (is (= (man/parse-manchester "(foo )")
           [:CLASS_EXPRESSION
            "(" [:CLASS_EXPRESSION [:LABEL "" "foo" ""]] " " ")"])))

  (testing "Disjunction"
    (is (= (man/parse-manchester "foo or bar")
           [:CLASS_EXPRESSION
            [:DISJUNCTION
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "or" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Conjunction"
    (is (= (man/parse-manchester "foo and bar")
           [:CLASS_EXPRESSION
            [:CONJUNCTION [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "and" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Negation"
    (is (= (man/parse-manchester "not foo")
           [:CLASS_EXPRESSION
            [:NEGATION
             "not" " "
             [:LABEL "" "foo" ""]]])))

  (testing "Some"
    (is (= (man/parse-manchester "'has part' some foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]]])))

  (testing "Some not"
    (is (= (man/parse-manchester "'has part' some not foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION
              [:NEGATION
               "not" " "
               [:LABEL "" "foo" ""]]]]])))

  (testing "Complex axiom"
    (is (= (man/parse-manchester "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION [:LABEL "'" "is about" "'"]]
             " " "some" "\n    "
             [:CLASS_EXPRESSION
              "("
              [:CLASS_EXPRESSION
               [:CONJUNCTION
                [:CLASS_EXPRESSION
                 [:LABEL "'" "material entity" "'"]]
                "\n     " "and" " "
                [:CLASS_EXPRESSION
                 "("
                 [:CLASS_EXPRESSION
                  [:SOME
                   [:OBJECT_PROPERTY_EXPRESSION
                    [:LABEL "'" "has role" "'"]]
                   " " "some" " "
                   [:CLASS_EXPRESSION
                    [:LABEL "'" "evaluant role" "'"]]]]
                 ")"]]]
              ")"]]])))

  (testing "Another complex axiom"
    (is (= (man/parse-manchester
            "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "" "has_specified_output" ""]]
             " " "some" "\n"
             [:CLASS_EXPRESSION
              "("
              [:CLASS_EXPRESSION
               [:CONJUNCTION
                [:CLASS_EXPRESSION
                 [:LABEL "'" "information content entity" "'"]]
                "\n " "and" " "
                [:CLASS_EXPRESSION
                 "("
                 [:CLASS_EXPRESSION
                  [:SOME
                   [:OBJECT_PROPERTY_EXPRESSION
                    [:LABEL "'" "is about" "'"]]
                   " " "some" "\n    "
                   [:CLASS_EXPRESSION
                    "("
                    [:CLASS_EXPRESSION
                     [:CONJUNCTION
                      [:CLASS_EXPRESSION
                       [:LABEL
                        "'" "material entity" "'"]]
                      "\n     " "and" " "
                      [:CLASS_EXPRESSION
                       "("
                       [:CLASS_EXPRESSION
                        [:SOME
                         [:OBJECT_PROPERTY_EXPRESSION
                          [:LABEL
                           "'" "has role" "'"]]
                         " " "some" " "
                         [:CLASS_EXPRESSION
                          [:LABEL "'" "evaluant role" "'"]]]]
                       ")"]]]
                    ")"]]]
                 ")"]]]
              ")"]]]))))

(defn round-trip? [str]
  (is (= str (man/manchester-format (man/parse-manchester str)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; nquads generation
(deftest test-->obj
  (testing "given a wrapped string, extracts it"
    (is (= "foo" (man/->obj [["foo"]]))))
  (testing "given a sequence of nquads, returns the subject"
    (is (= "foo" (man/->obj [[:graph "foo" "bar" "baz"]])))))

;; TODO - test expression->nquads
