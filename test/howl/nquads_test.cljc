(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]
            [howl.nquads :refer :all]))

(deftest test-statements->urls
  (testing "Takes a list of statements and returns a lazy list containing only the URLs in those statements"
    (is (= ["http://example.com"]
           (statements->urls
            [["_:foo" "_:bar" "http://example.com"]
             ["_:baz"]]))))
  (testing "Ignores the default-graph node"
    (is (= ["http://example.com"]
           (statements->urls
            [["_:foo" "_:bar" "http://example.com"]
             ["_:baz" default-graph]]))))
  (testing "Returns multiple instances of the same URL if found"
    (is (= ["http://example.com" "http://example.com"]
           (statements->urls
            [["_:foo" "_:bar" "http://example.com"]
             ["_:baz" default-graph "http://example.com"]])))))

(deftest test-partition-url
  (testing "Splits given URLS, maintaining delimiters"
    (is (= ["http:" "/" "/" "example.com" "/" "foo"]
           (partition-url "http://example.com/foo")))
    (is (= ["http:" "/" "/" "example.com" "/" "foo" "#" "bar"]
           (partition-url "http://example.com/foo#bar")))))

(deftest test-url->prefixes
  (testing "Returns all relevant prefixes of the given URL, not including the input"
    (is (nil? (url->prefixes "http://example.com")))
    (is (= ["http://example.com/"]
           (url->prefixes "http://example.com/foo")))
    (is (= ["http://example.com/" "http://example.com/foo/"]
           (url->prefixes "http://example.com/foo/bar")))
    (is (= ["http://example.com/" "http://example.com/foo/" "http://example.com/foo/bar#"]
           (url->prefixes "http://example.com/foo/bar#baz")))))

(deftest test-url->prefix-name
  (testing "Given a URL with no #-component, return the last path element"
    (is (= "foo" (url->prefix-name "http://example.com/foo"))))
  (testing "Given a URL with a #-component, return it"
    (is (= "bar" (url->prefix-name "http://example.com/foo#bar")))))

(deftest test-unique-assoc
  (testing "Given a map and key/value not already in it, default to assoc"
    (is (= {"a" 1} (unique-assoc {} "a" 1)))
    (is (= {"a" 1 "b" 2} (unique-assoc {"a" 1} "b" 2))))
  (testing "Given a map and key/value already in it, append a numeric suffix to the key"
    (is (= {"a" 1 "a-2" 2} (unique-assoc {"a" 1} "a" 2))))
  (testing "Handle chain collisions by incrementing the numeric suffix until a non-colliding key is found"
    (is (= {"a" 1 "a-2" 2 "a-3" 3} (unique-assoc {"a" 1 "a-2" 2} "a" 3)))
    (is (= {"a" 1 "a-2" 2 "a-3" 3 "a-4" 4 "a-5" 5}
           (unique-assoc {"a" 1 "a-2" 2 "a-3" 3 "a-4" 4} "a" 5)))))

(deftest test-statements->prefixes
  (testing "Does not include urls only domains"
    (is (= {} (statements->prefixes [["_:foo" "_:bar" "http://example.com/"]]))))
  (testing "Includes URLs with at least one path element"
    (is (= {"example" "http://example.com/"}
           (statements->prefixes [["_:foo" "_:bar" "http://example.com/foo"]]))))
  (testing "For URLs with multiple path elements, use the last included one as a label"
    (is (= {"foo" "http://example.com/foo/" "example" "http://example.com/"}
           (statements->prefixes [["_:foo" "_:bar" "http://example.com/foo/bar"]])))
    (is (= {"foo" "http://example.com/foo#" "example" "http://example.com/"}
           (statements->prefixes [["_:foo" "_:bar" "http://example.com/foo#bar"]])))))

(deftest test-statements->labels
  (testing "Does not suggest labels that occur fewer than three times"
    (is (= {"example" "http://example.com/"}
           (nquads/statements->labels
            [["_:foo" "_:bar" "http://example.com/foo/bar"]
             ["_:foo" "_:bar" "http://example.com/foo/bar"]
             ["_:foo" "_:bar" "http://example.com/foo/bar/baz"]
             ["_:foo" "_:bar" "http://example.com/"]
             ["_:foo" "_:bar" "http://example.com/"]
             ["_:foo" "_:bar" "http://example.com/"]])))))

;; (deftest test-convert
;;   (testing "literal"
;;     (is (= (convert-quads
;;             {:current-subject "http://foo.com/foo"
;;              :block
;;              {:block-type :LITERAL_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdfs "label")]
;;               :value "foo"
;;               :lang "en"}})
;;            {:current-subject "http://foo.com/foo"
;;             :block
;;             {:block-type :LITERAL_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdfs "label")]
;;              :value "foo"
;;              :lang "en"}
;;             :statements
;;             [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]
;;             :quads
;;             [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]})))
;;   (testing "literal default type"
;;     (is (= (convert-quads
;;             {:iri-type {(str rdfs "label") {:lang "en"}}
;;              :current-subject "http://foo.com/foo"
;;              :block
;;              {:block-type :LITERAL_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdfs "label")]
;;               :value "foo"}})
;;            {:iri-type {(str rdfs "label") {:lang "en"}}
;;             :current-subject "http://foo.com/foo"
;;             :block
;;             {:block-type :LITERAL_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdfs "label")]
;;              :value "foo"}
;;             :statements
;;             [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]
;;             :quads
;;             [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]]})))
;;   (testing "link"
;;     (is (= (convert-quads
;;             {:current-subject "http://foo.com/foo"
;;              :block
;;              {:block-type :LINK_BLOCK
;;               :predicate [:ABSOLUTE_IRI (str rdf "type")]
;;               :object [:ABSOLUTE_IRI (str owl "Class")]}})
;;            {:current-subject "http://foo.com/foo"
;;             :block
;;             {:block-type :LINK_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdf "type")]
;;              :object [:ABSOLUTE_IRI (str owl "Class")]}
;;             :statements
;;             [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]
;;             :quads
;;             [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]})))
;;   (testing "nested link annotation"
;;     (is (= (convert-quads
;;             {:block
;;              {:block-type :LINK_BLOCK
;;               :arrows ">>"
;;               :predicate [:ABSOLUTE_IRI (str rdfs "seeAlso")]
;;               :object [:ABSOLUTE_IRI (str owl "Class")]}
;;              :statements
;;              ["A"
;;               [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
;;               "C"
;;               "D"
;;               "E"]})
;;            {:block
;;             {:block-type :LINK_BLOCK
;;              :arrows ">>"
;;              :predicate [:ABSOLUTE_IRI (str rdfs "seeAlso")]
;;              :object [:ABSOLUTE_IRI (str owl "Class")]}
;;             :blank-node-count 1
;;             :statements
;;             ["A"
;;              [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
;;              [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]]
;;             :quads
;;             [[nil "_:b0" (str rdf "type") (str owl "Axiom")]
;;              [nil "_:b0" (str owl "annotatedSource")   "http://foo.com/foo"]
;;              [nil "_:b0" (str owl "annotatedProperty") (str rdf "type")]
;;              [nil "_:b0" (str owl "annotatedTarget")   (str owl "Class")]
;;              [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]]})))
;;   (testing "expression label"
;;     (is (= (convert-quads
;;             {:current-subject "http://foo.com/foo"
;;              :block
;;              {:block-type :EXPRESSION_BLOCK
;;               :predicate [:ABSOLUTE_IRI (str rdf "type")]
;;               :expression [:NAME [:ABSOLUTE_IRI (str owl "Class")]]}})
;;            {:current-subject "http://foo.com/foo"
;;             :block
;;             {:block-type :EXPRESSION_BLOCK
;;              :predicate [:ABSOLUTE_IRI (str rdf "type")]
;;              :expression [:NAME [:ABSOLUTE_IRI (str owl "Class")]]}
;;             :statements
;;             [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]
;;             :quads
;;             [[nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]]}))))


;; (deftest test-quad-to-string
;;   (testing "basics"
;;     (is (= (map
;;             quad-to-string
;;             [[nil "http://foo.com/foo" (str rdfs "label") {:value "foo" :lang "en"}]
;;              [nil "http://foo.com/foo" (str rdf "type") (str owl "Class")]
;;              [nil "_:b0" (str rdf "type") (str owl "Axiom")]
;;              [nil "_:b0" (str owl "annotatedSource")   "http://foo.com/foo"]
;;              [nil "_:b0" (str owl "annotatedProperty") (str rdf "type")]
;;              [nil "_:b0" (str owl "annotatedTarget")   (str owl "Class")]
;;              [nil "_:b0" (str rdfs "seeAlso") (str owl "Class")]])
;;            ["<http://foo.com/foo> <http://www.w3.org/2000/01/rdf-schema#label> \"foo\"@en ."
;;             "<http://foo.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> ."
;;             "_:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> ."
;;             "_:b0 <http://www.w3.org/2002/07/owl#annotatedSource> <http://foo.com/foo> ."
;;             "_:b0 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ."
;;             "_:b0 <http://www.w3.org/2002/07/owl#annotatedTarget> <http://www.w3.org/2002/07/owl#Class> ."
;;             "_:b0 <http://www.w3.org/2000/01/rdf-schema#seeAlso> <http://www.w3.org/2002/07/owl#Class> ."]))))
