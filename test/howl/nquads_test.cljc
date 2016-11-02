(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]

            [howl.nquads :refer :all]
            [howl.util :refer [rdf> owl>]]))

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
           (statements->labels
            [["_:foo" "_:bar" "http://example.com/foo/bar"]
             ["_:foo" "_:bar" "http://example.com/foo/bar"]
             ["_:foo" "_:bar" "http://example.com/foo/bar/baz"]
             ["_:foo" "_:bar" "http://example.com/"]
             ["_:foo" "_:bar" "http://example.com/"]
             ["_:foo" "_:bar" "http://example.com/"]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pull out annotations
(def subject-map
  {(rdf> "type") {(owl> "Axiom") true}
   (owl> "annotatedSource") {"foo" true}
   (owl> "annotatedProperty") {"bar" true}
   (owl> "annotatedTarget") {"baz" true}})

(deftest test-annotation?
  (testing "annotation? returns true for annotation subject/predicate maps"
    (is (annotation? "_:b1" subject-map)))
  (testing "annotation? returns false for things that don't have blank node names"
    (is (not (annotation? "foobar" subject-map))))
  (testing "annotation? returns false for things that aren't owl Axioms, or
that are missing any of the annotated* statements"
    (is (not (annotation? "_:b1" (assoc subject-map (rdf> "type") {"foobar" true}))))
    (is (not (annotation? "_:b1" (dissoc subject-map (owl> "annotatedSource")))))
    (is (not (annotation? "_:b1" (dissoc subject-map (owl> "annotatedProperty")))))
    (is (not (annotation? "_:b1" (dissoc subject-map (owl> "annotatedTarget")))))))

(deftest test-separate-annotations
  (testing "takes a subject map and separates out annotations.
Returns a pair of maps
  - the input map with all annotations removed
  - a map containing only the annotations from the input map"
    (is (= [{"foo" {"bar" {"baz" true}}} {"_:b1" subject-map}]
           (separate-annotations
            {"_:b1" subject-map "foo" {"bar" {"baz" true}}})))))

(deftest test-annotations-for
  (testing "takes a subject/predicate/object tuple and returns a list of subject/predicate-maps
represeting annotations targeting it from the given annotations map"
    (is (= [["_:b1" subject-map]
            ["_:b2" subject-map]]
           (annotations-for
            ["foo" "bar" "baz"]
            {"_:b1" subject-map
             "_:b2" subject-map
             "_:b3"
             {(rdf> "type") {(owl> "Axiom") true}
              (owl> "annotatedSource") {"flarp" true}
              (owl> "annotatedProperty") {"blarg" true}
              (owl> "annotatedTarget") {"bleep" true}}})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Output howl AST
(deftest test-invert-env
  (testing "Inverting an environment means inverting its :labels and :prefixes values"
    (is (= {:labels {"bar" "foo"} :prefixes {"mumble" "baz"}}
           (invert-env {:labels {"foo" "bar"} :prefixes {"baz" "mumble"}})))))

(deftest test-longest-prefix
  (testing "if the target string is present among candidates, return it"
    (is (= "batman" (longest-prefix "batman" ["b" "bat" "batm" "batma" "batman"]))))
  (testing "return the longest prefix otherwise"
    (is (= "batma" (longest-prefix "batman" ["b" "bat" "batm" "batma"]))))
  (testing "do not return words that are not prefixes"
    (is (= "batma" (longest-prefix "batman" ["b" "bat" "foo" "batm" "batma" "bar" "foobarbazmumble"])))))

(deftest test-leaf-node
  (testing "given blank name, return a BLANK_NODE_LABEL"
    (is (= [:BLANK_NODE_LABEL "_:foo"] (leaf-node {} "_:foo"))))
  (testing "given an IRI that has no label or prefix component, return an IRIREF"
    (is (= [:IRIREF "<" "http://example.com/foo" ">"]
           (leaf-node {} "http://example.com/foo"))))
  (testing "if the given IRI is specified as the value of a label in the given env
return a LABEL instead"
    (is (= [:LABEL "foo"]
           (leaf-node
            {:labels {"foo" "http://example.com/foo"}}
            "http://example.com/foo"))))
  (testing "if the given env contains a prefix fo the given IRI,
return a PREFIXED_NAME instead"
    (is (= [:PREFIXED_NAME [:PREFIX "ex"] ":" "foo"]
           (leaf-node
            {:prefixes {"ex" "http://example.com/"}}
            "http://example.com/foo"))))
  (testing "if more than one prefix matches the given IRI, use the longest"
    (is (= [:PREFIXED_NAME [:PREFIX "foo"] ":" "bar"]
           (leaf-node
            {:prefixes
             {"ex" "http://example.com/"
              "foo" "http://example.com/foo/"}}
            "http://example.com/foo/bar")))))

(deftest test-render-literal
  (testing "makes no changes to one-line literals"
    (is (= "foobarbaz" (render-literal "foobarbaz"))))
  (testing "for multi-line literals, prepend all lines but the first with '  '"
    (is (= "foo\n  bar\n  baz" (render-literal "foo\nbar\nbaz")))))

(deftest test-collapse
  (testing "collapses triples appropriately"
    (is (= {:foo {:bar {:baz nil
                        :mumble nil
                        2 nil}
                  1 {2 nil}}
            :bleargh {:flarp {:floop nil}}}
           (collapse
            [[:foo :bar :baz]
             [:foo :bar :mumble]
             [:foo :bar 2]
             [:foo 1 2]
             [:bleargh :flarp :floop]]))))
  (testing "collapses quads appropriately"
    (is (= {:foo {:bar {:baz {:mumble nil
                              :foobar nil}
                        :flarp {:foobar nil}}
                  :mumble {:baz {:foobar nil}}}
            :bar {:baz {:mumble {:foo nil}}}}
           (collapse
            [[:foo :bar :baz :mumble]
             [:foo :bar :baz :foobar]
             [:foo :bar :flarp :foobar]
             [:foo :mumble :baz :foobar]
             [:bar :baz :mumble :foo]])))))

;; TODO
;;(deftest test-render-annotation-tree)

(deftest test-render-predicates
  (testing "given a string object, return a LITERAL_BLOCK"
    (is (= [[:LINK_BLOCK
             [:PREDICATE [:IRIREF "<" "bar" ">"]]
             [:COLON_ARROW "" ":>" " "]
             [:OBJECT [:IRIREF "<" "http://example.com" ">"]]]]
           (render-predicates {} {"bar" {"http://example.com" nil}} "foo" {} ""))))
  (testing "given a map object (which is how jena encodes literal values), return a LITERAL_BLOCK"
    (is (= [[:LITERAL_BLOCK
             [:PREDICATE [:IRIREF "<" "bar" ">"]]
             [:COLON "" ":" " "]
             [:LITERAL "baz"]]]
           (render-predicates {} {"bar" {{:value "baz"} nil}} "foo" {} ""))))
  (testing "given a map object with :lang and :type properties, generate appropriate sub-trees
for Howl LANGUAGE and TYPE annotations"
    (is (= [[:LITERAL_BLOCK
             [:PREDICATE [:IRIREF "<" "bar" ">"]]
             [:LANGUAGE [:SPACES " "] "en"]
             [:TYPE [:SPACES " "] [:DATATYPE [:IRIREF "string"]]]
             [:COLON "" ":" " "]
             [:LITERAL "baz"]]]
           (render-predicates {} {"bar" {{:value "baz" :lang "en" :type "string"} nil}} "foo" {} ""))))
  ;; TODO - test that it generates annotations when passed a non-empty annotations map
  )

;; TODO
;; (deftest test-render-subjects)
;; (deftest test-render-graphs)
