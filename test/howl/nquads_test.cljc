(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]
            [howl.nquads :refer :all]))

(def test-state
  {:base "http://example.com/"
   :prefixes {"ex" "http://example.com/"}
   :labels {"foo" "http://example.com/foo"}})

(deftest test-resolve-name
  (testing "absolute IRI"
    (is (= (resolve-name
            test-state
            {}
            [:IRI "<" "http://example.com/foo" ">"])
           "http://example.com/foo")))
  (testing "relative IRI"
    (is (= (resolve-name
            test-state
            {}
            [:IRI "<" "foo" ">"])
           "http://example.com/foo")))
  (testing "prefixed name"
    (is (= (resolve-name
            test-state
            {}
            [:PREFIXED_NAME "ex" ":" "foo"])
           "http://example.com/foo")))
  (testing "label"
    (is (= (resolve-name
            test-state
            {}
            [:LABEL "foo"])
           "http://example.com/foo"))))

(deftest test-format-literal
  (testing "plain literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "plain"})
           "\"plain\"")))
  (testing "language literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "English"
             :language "@en"})
           "\"English\"@en")))
  (testing "TYPE language literal"
    (is (= (format-literal
            (assoc-in
             test-state
             [:types-language "http://example.com/foo"]
             "@en")
            {:predicate [:LABEL "foo"]
             :content "English"})
           "\"English\"@en")))
  (testing "datatype literal"
    (is (= (format-literal
            test-state
            {:predicate [:LABEL "foo"]
             :content "FOO"
             :datatype [:LABEL "foo"]})
           "\"FOO\"^^<http://example.com/foo>")))
  (testing "TYPE datatype literal"
    (is (= (format-literal
            (assoc-in
             test-state
             [:types-datatype "http://example.com/foo"]
             "http://example.com/foo")
            {:predicate [:LABEL "foo"]
             :content "FOO"})
           "\"FOO\"^^<http://example.com/foo>"))))

(deftest test-render-block
  (testing "BASE"
    (let [[state quads]
          (render-block
           {}
           {:block-type :BASE_BLOCK
            :iri "http://example.com/"})]
      (is (= state {:base "http://example.com/"}))
      (is (nil? quads))))
  (testing "PREFIX"
    (let [[state quads]
          (render-block
           {}
           {:block-type :PREFIX_BLOCK
            :prefix "ex"
            :iri "http://example.com/"})]
      (is (= state {:prefixes {"ex" "http://example.com/"}}))
      (is (nil? quads))))
  (testing "LABEL"
    (let [[state quads]
          (render-block
           {}
           {:block-type :LABEL_BLOCK
            :label "foo"
            :identifier [:IRI "<" "http://example.com/foo" ">"]})]
      (is (= state {:labels {"foo" "http://example.com/foo"}}))
      (is (nil? quads))))
  (testing "TYPE"
    (let [[state quads]
          (render-block
           {}
           {:block-type :TYPE_BLOCK
            :predicate [:IRI "<" "http://example.com/foo" ">"]
            :language "en"})]
      (is (= state {:types-language {"http://example.com/foo" "en"}}))
      (is (nil? quads))))
  (testing "GRAPH"
    (let [[state quads]
          (render-block
           {}
           {:block-type :GRAPH_BLOCK
            :graph [:IRI "<" "http://example.com/foo" ">"]})]
      (is (= state
             {:graph "<http://example.com/foo>"
              :subjects [["<http://example.com/foo>" "<http://example.com/foo>"]]}))
      (is (nil? quads))))
  (testing "Subject"
    (let [[state quads]
          (render-block
           {}
           {:block-type :SUBJECT_BLOCK
            :subject [:IRI "<" "http://example.com/foo" ">"]})]
      (is (= state {:subjects [[nil "<http://example.com/foo>"]]}))
      (is (nil? quads))))
  (testing "Literal"
    (let [[state quads]
          (render-block
           {:subjects [[nil "<http://example.com/foo>"]]}
           {:block-type :LITERAL_BLOCK
            :predicate [:IRI "<" "http://example.com/foo" ">"]
            :content "FOO"})]
      (is (= state
             {:subjects
              [[nil "<http://example.com/foo>" "<http://example.com/foo>" "\"FOO\""]]}))
      (is (= quads
             [[nil "<http://example.com/foo>" "<http://example.com/foo>" "\"FOO\""]]))))
  (testing "Link"
    (let [[state quads]
          (render-block
           {:subjects [[nil "<http://example.com/foo>"]]}
           {:block-type :LINK_BLOCK
            :predicate [:IRI "<" "http://example.com/foo" ">"]
            :object [:IRI "<" "http://example.com/bar" ">"]})]
      (is (= state
             {:subjects
              [[nil
                "<http://example.com/foo>"
                "<http://example.com/foo>"
                "<http://example.com/bar>"]]}))
      (is (= quads
             [[nil
               "<http://example.com/foo>"
               "<http://example.com/foo>"
               "<http://example.com/bar>"]]))))
  (testing "Annotation"
    (let [[state quads]
          (render-block
           {:subjects
            [[nil
              "<http://example.com/foo>"
              "<http://example.com/foo>"
              "<http://example.com/bar>"]]}
           {:block-type :LITERAL_BLOCK
            :arrows ">"
            :predicate [:IRI "<" "http://example.com/foo" ">"]
            :content "FOO"})]
      (is (= state
             {:blank-node-count 1
              :subjects
              [[nil
                "<http://example.com/foo>"
                "<http://example.com/foo>"
                "<http://example.com/bar>"]
               [nil "_:b1" "<http://example.com/foo>" "\"FOO\""]]}))
      (is (= quads
             [[nil
               "_:b1"
               "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
               "<http://www.w3.org/2002/07/owl#Axiom>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedSource>"
               "<http://example.com/foo>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedProperty>"
               "<http://example.com/foo>"]
              [nil
               "_:b1"
               "<http://www.w3.org/2002/07/owl#annotatedTarget>"
               "<http://example.com/bar>"]
              [nil
               "_:b1"
               "<http://example.com/foo>"
               "\"FOO\""]]))))
  ; more
  )
