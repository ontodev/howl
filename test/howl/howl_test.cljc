(ns howl.howl-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [clojure.string :as string]
            [instaparse.core :as insta]

            [howl.util :as util :refer [<>]]
            [howl.howl :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing
(deftest test-group-lines
  (testing "creates singleton groups from standalone lines"
    (is (= '(("foo") ("bar") ("baz"))
           (group-lines ["foo" "bar" "baz"]))))

  (testing "groups consecutive lines that start with two spaces"
    (is (= '(("foo" "  bar") ("baz"))
           (group-lines ["foo" "  bar" "baz"])))))


#_(deftest test-top-level-productions
  (testing "COMMENT_BLOCK"
    (is (= (block-parser "# just a comment")
           [[:COMMENT_BLOCK "# " "just a comment"]
            [:TRAILING_WHITESPACE ""]]))
    (is (= (block-parser "### another comment")
           [[:COMMENT_BLOCK "### " "another comment"]
            [:TRAILING_WHITESPACE ""]])))
  (testing "BASE_BLOCK"
    (is (= (block-parser "BASE <http://example.com/>")
           [[:BASE_BLOCK
             "BASE" [:SPACES " "]
             [:BASE [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "e" "x" "a" "m" "p" "l" "e" "." "c" "o" "m" "/" ">"]]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "PREFIX_BLOCK"
    (is (= (block-parser "PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
           [[:PREFIX_BLOCK "PREFIX"
             [:SPACES " "]
             [:PREFIX "rdf"] [:COLON_ARROW "" ":>" " "]
             [:PREFIXED [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "LABEL_BLOCK"
    (is (= (block-parser "LABEL rdf:type: type")
           [[:LABEL_BLOCK "LABEL" [:SPACES " "]
             [:SUBJECT
              [:PREFIXED_NAME
               [:PREFIX "rdf"] ":" "type"]] [:COLON "" ":" " "]
             [:LABEL "type"]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "DEFAULT_BLOCK"
    (is (= (block-parser "DEFAULT label LANGUAGE en")
           [[:DEFAULT_BLOCK
             "DEFAULT" [:SPACES " "] [:PREDICATE [:LABEL "label"]] [:SPACES " "]
             "LANGUAGE" [:SPACES " "]
             [:LANGUAGE "en"]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "GRAPH_BLOCK"
    (is (= (block-parser "GRAPH ex:graph")
           [[:GRAPH_BLOCK
             "GRAPH" [:SPACES " "]
             [:GRAPH [:PREFIXED_NAME [:PREFIX "ex"] ":" "graph"]]]
            [:TRAILING_WHITESPACE ""]]))
    (is (= (block-parser "GRAPH")
           [[:GRAPH_BLOCK "GRAPH"]
            [:TRAILING_WHITESPACE ""]])))
  (testing "SUBJECT_BLOCK"
    (is (= (block-parser "ex:ontology")
           [[:SUBJECT_BLOCK
             [:SUBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "ontology"]]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "ANNOTATION"
    (is (= (block-parser "> comment TYPE xsd:string: An annotation on a comment.")
           [[:ANNOTATION
             [:ARROWS ">" " "]
             [:LITERAL_BLOCK
              [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
              "TYPE" [:SPACES " "] [:DATATYPE [:PREFIXED_NAME [:PREFIX "xsd"] ":" "string"]]
              [:COLON "" ":" " "] [:LITERAL "An annotation on a comment."]]]
            [:TRAILING_WHITESPACE ""]]))
    (is (= (block-parser ">>> comment LANGUAGE en: An annotation on a comment.")
           [[:ANNOTATION
             [:ARROWS ">>>" " "]
             [:LITERAL_BLOCK
              [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
              "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"]
              [:COLON "" ":" " "] [:LITERAL "An annotation on a comment."]]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "LITERAL_BLOCK"
    (is (= (block-parser "comment LANGUAGE en: A comment on 'Foo'.")
           [[:LITERAL_BLOCK
             [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
             "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"] [:COLON "" ":" " "]
             [:LITERAL "A comment on 'Foo'."]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "LINK_BLOCK"
    (is (= (block-parser "rdfs:seeAlso:> ex:bat")
           [[:LINK_BLOCK
             [:PREDICATE [:PREFIXED_NAME [:PREFIX "rdfs"] ":" "seeAlso"]]
             [:COLON_ARROW "" ":>" " "]
             [:OBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "bat"]]]
            [:TRAILING_WHITESPACE ""]])))
  (testing "EXPRESSION_BLOCK"
    (is (= (block-parser "subclass of TYPE OMN:> 'has part' some Foo")
           [[:EXPRESSION_BLOCK
             [:PREDICATE [:LABEL "subclass" [:SPACES " "] "of"]] [:SPACES " "]
             "TYPE" [:SPACES " "] [:DATATYPE [:LABEL "OMN"]]
             [:COLON_ARROW "" ":>" " "]
             [:EXPRESSION "'has part' some Foo"]]
            [:TRAILING_WHITESPACE ""]]))))

(def bad-labels
 ["DEFAULT foo"
  "GRAPH"
  "PREFIX bar: <baz>"
  "gru TYPE integer"
  "gru LANGUAGE integer"
  "# comment"
  ">> annotation"
  "trailing space "
  "trailing colon:"
  "trailing colon arrow:>"])

#_(deftest test-labels-parse
 (testing "Bad labels either cause an outright parse failure, or return a parse tree that does not designate a :LABEL_BLOCK"
   (doseq [bad-label bad-labels]
     (is (or (insta/failure? (block-parser bad-label))
             (not= :LABEL_BLOCK (first (block-parser bad-label))))))))

#_(deftest test-lines->blocks
  (testing "returns a {:exp ParseTree} map, with source and line count in metadata"
    (is (= (first (lines->blocks ["PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"]))
           {:exp [:PREFIX_BLOCK "PREFIX"
                  [:SPACES " "] [:PREFIX "rdf"] [:COLON_ARROW "" ":>" " "]
                  [:PREFIXED
                   [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]]]}))
    (is (= (meta (first (lines->blocks ["PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"])))
           {:origin {:name "interactive", :line 1}}))))

#_(deftest test-parse-tree->string
  (testing "returns a string unmodified"
    (is (= "foo" (parse-tree->string "foo"))))

  (testing "for :ABSOLUTE_IRI blocks, returns the third element"
    (is (= "foo" (parse-tree->string [:ABSOLUTE_IRI "<" "foo" ">"]))))

  (testing "for :LABEL, :PREFIX, :TRAILING_WHITESPACE, and :SPACES blocks, returns the second element"
    (is (= "foo" (parse-tree->string [:LABEL "foo"])))
    (is (= "foo" (parse-tree->string [:PREFIX "foo"])))
    (is (= "foo" (parse-tree->string [:TRAILING_WHITESPACE "foo"])))
    (is (= "foo" (parse-tree->string [:SPACES "foo"]))))

  (testing "for other forms, joins the recursion on rest"
    (is (= "foobar" (parse-tree->string [:BLAH [:LABEL "foo"] "bar"])))
    (is (= "foo bar" (parse-tree->string [:BLAH [:LABEL "foo"] [:SPACES " "] [:BLEEH [:BLUH "bar"]]])))))

#_(deftest test-lines<->blocks
  (testing "calling block->string on an element of the return value of lines->blocks returns the string we started with"
    (is (let [ln "PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"]
          (= ln (block->string (first (lines->blocks [ln]))))))))

#_(deftest test-condense-tree
  (testing "condenses :IRIREF blocks"
    (is (= [:IRIREF "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]
           (condense-tree [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]))))
  (testing "returns strings, keywords, NILs and numbers as-is"
    (is (= 123 (condense-tree 123)))
    (is (= "foo" (condense-tree "foo")))
    (is (= :foo (condense-tree :foo)))
    (is (= nil (condense-tree nil))))
  (testing "returns :TRAILING_WHITESPACE blocks as-is"
    (is (= [:TRAILING_WHITESPACE "" "" "" ""]
           (condense-tree [:TRAILING_WHITESPACE "" "" "" ""]))))
  (testing "recurs down other trees"
    (is (= [:FOO
            [:BAR 8237]
            [:BAR
             [:IRIREF "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]]
            [:BAZ
             [:QUUX
              [:MUMBLE :foo nil]
              [:IRIREF "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]]]]
           (condense-tree
            [:FOO
             [:BAR 8237]
             [:BAR
              [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]]
             [:BAZ
              [:QUUX
               [:MUMBLE :foo nil]
               [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]]]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Environment extraction
#_(deftest test-name-from-node
  (testing "get the name out of a blank node"
    (is (= "testing"
           (name-from-node
            {} [:BLANK_NODE "_:" "testing"]))))
  (testing "get the IRI out of an absolute IRI"
    (is (= "http://example.com/"
           (name-from-node
            {} [:ABSOLUTE_IRI "<" "http://example.com/" ">"]))))
  (testing "get the word out of a :WORD"
    (is (= "foobar"
           (name-from-node
            {} [:WORD "foobar"]))))
  (testing "get the IRI out of a wrapped IRI"
    (is (= "http://example.com/"
           (name-from-node
            {} [:IRIREF "<" "http://example.com/" ">"]))))
  (testing "expand the prefix and get the resulting absolute IRI out of a prefixed name"
    (is (= "http://example.com/subClassOf"
           (name-from-node
            {:prefixes {"ex" "http://example.com/"}}
            [:PREFIXED_NAME [:PREFIX "ex"] ":" "subClassOf"] ))))
  (testing "recur into :SUBJECT"
    (is (= "http://example.com/"
           (name-from-node
            {} [:SUBJECT [:ABSOLUTE_IRI "<" "http://example.com/" ">"]])))))

#_(deftest test-locate
  (testing "when there is an :origin property in the targets meta, return it"
    (is (= (locate ^{:origin {:name "local" :line 3}} [:BLOCK])
           {:name "local" :line 3})))
  (testing "when there is no :origin in the targets meta, return the stringified target"
    (is (= (locate [:BLOCK]) (str [:BLOCK])))))

#_(deftest test-parse-tree->names
  (testing "a :PREFIX_BLOCK returns new prefixes"
    (is (= {:prefixes {"ex" "http://example.com/"}}
           (parse-tree->names
            {} [:PREFIX_BLOCK
                "PREFIX" [:SPACES " "] [:PREFIX "ex"] [:COLON_ARROW "" ":>" " "]
                [:PREFIXED [:IRIREF "<" "http://example.com/" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :LABEL_BLOCK returns new labels"
    (is (= {:labels {"label" "http://example.com/"}}
           (parse-tree->names
            {} [:LABEL_BLOCK
                "LABEL" [:SPACES " "] [:SUBJECT [:IRIREF "<" "http://example.com/" ">"]] [:COLON "" ":" " "]
                [:LABEL "label"]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :DEFAULT_BLOCK can return new default values for TYPE"
    (is (= {:defaults {"http://example.com/foo" {"TYPE" "http://example.com/bar"}}}
           (parse-tree->names
            {} [:DEFAULT_BLOCK
                "DEFAULT" [:SPACES " "] [:PREDICATE [:IRIREF "<" "http://example.com/foo" ">"]] [:SPACES " "]
                "TYPE" [:SPACES " "] [:DATATYPE [:IRIREF "<" "http://example.com/bar" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :DEFAULT_BLOCK can return a new default value for LANUGAGE"
    (is (= {:defaults {"http://example.com/foo" {"LANGUAGE" "en"}}}
           (parse-tree->names
            {} [:DEFAULT_BLOCK
                "DEFAULT" [:SPACES " "] [:PREDICATE [:IRIREF "<" "http://example.com/foo" ">"]] [:SPACES " "]
                "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :SUBJECT_BLOCK returns a new :subject"
    (is (= {:subject "http://example.com/"}
           (parse-tree->names
            {} [:SUBJECT_BLOCK
                [:SUBJECT [:IRIREF "<" "http://example.com/" ">"]]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :BASE_BLOCK returns a new :base"
    (is (= {:base "http://example.com/"}
           (parse-tree->names
            {} [:BASE_BLOCK
                "BASE" [:SPACES " "]
                [:BASE [:IRIREF "<" "http://example.com/" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :GRAPH_BLOCK returns a new :graph and a new :subject (both the given graph)"
    (is (= {:graph "http://example.com/graph", :subject "http://example.com/graph"}
           (parse-tree->names
            {} [:GRAPH_BLOCK
                "GRAPH" [:SPACES " "]
                [:GRAPH [:IRIREF "<" "http://example.com/graph" ">"]]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "when the target or label is a prefixed value, or label, it is resolved"
    (is (= {:subject "http://example.com/foo"}
           (parse-tree->names
            {:prefixes {"ex" "http://example.com/"}}
            [:SUBJECT_BLOCK
             [:SUBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "foo"]]
             [:TRAILING_WHITESPACE ""]])))
    (is (= {:subject "http://example.com/foo"}
           (parse-tree->names
            {:labels {"foo" "http://example.com/foo"}}
            [:SUBJECT_BLOCK
             [:SUBJECT [:LABEL "foo"]]
             [:TRAILING_WHITESPACE ""]]))))

  (testing "other trees don't affect the environment, so return empty maps"
    (is (= {} (parse-tree->names {} [:BLANK_BLOCK ""])))
    (is (= {} (parse-tree->names {} [:COMMENT_BLOCK ""])))
    (is (= {} (parse-tree->names {} [:ANNOTATION ""])))
    (is (= {} (parse-tree->names {} [:LITERAL_BLOCK ""])))
    (is (= {} (parse-tree->names {} [:LINK_BLOCK ""])))
    (is (= {} (parse-tree->names {} [:EXPRESSION_BLOCK ""])))))

#_(deftest test-merge-environments
  (testing "merges :prefixes and :labels with merge"
    (is (let [res (merge-environments
                   {:labels {:a "foo"} :prefixes {:b "bar"}}
                   {:labels {:c "baz"} :prefixes {:b "foo"}})]
          (and (= {:a "foo" :c "baz"} (res :labels))
               (= {:b "foo"} (res :prefixes))))))
  (testing "merges :subject and :base by taking the more recent value"
    (is (let [res (merge-environments
                   {:subject "foo" :base "bar"}
                   {:subject nil :base "baz"})]
          (and (= "foo" (res :subject))
               (= "baz" (res :base)))))
    (is (let [res (merge-environments {:subject "foo" :base "bar"} {:base "baz"})]
          (and (= "foo" (res :subject))
               (= "baz" (res :base))))))
  (testing "merges :graph only if the new environment has a graph entry"
    (is (= "foo"
           ((merge-environments {:graph "foo"} {}) :graph)))
    (is (= "bar"
           ((merge-environments {:graph "foo"} {:graph "bar"}) :graph))))
  (testing "merges :defaults with a recursive merge"
    (is (= {"foo" {"LANGUAGE" "en" "TYPE" "baz"} "bar" {"LANGUAGE" "fr"}}
           ((merge-environments
             {:defaults {"foo" {"LANGUAGE" "en"}}}
             {:defaults {"foo" {"TYPE" "baz"} "bar" {"LANGUAGE" "fr"}}})
            :defaults)))))

(def +test-blocks+
  "Generated with

   (take 32
     (->> (line-seq (clojure.java.io/reader \"test/test1.howl\"))
          (#(core/lines->blocks %))
          (map core/condense-chars)
          (map core/parse-expressions)))

  That's not inlined, because it doesn't work in JS (which is a goal as of this writing)"
  [{:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "rdf"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]]]}
   {:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "rdfs"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://www.w3.org/2000/01/rdf-schema#" ">"]]]}
   {:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "xsd"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://www.w4.org/2001/XMLSchema#" ">"]]]}
   {:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "owl"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://www.w3.org/2002/07/owl#" ">"]]]}
   {:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "obo"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://purl.obolibrary.org/obo/" ">"]]]}
   {:exp [:PREFIX_BLOCK
          "PREFIX" [:SPACES " "]
          [:PREFIX "ex"] [:COLON_ARROW "" ":>" " "]
          [:PREFIXED [:IRIREF "<" "http://example.com/" ">"]]]}
   {:exp [:LABEL_BLOCK
          "LABEL" [:SPACES " "]
          [:SUBJECT [:PREFIXED_NAME [:PREFIX "rdf"] ":" "type"]] [:COLON "" ":" " "]
          [:LABEL "type"]]}
   {:exp [:LABEL_BLOCK
          "LABEL" [:SPACES " "]
          [:SUBJECT [:PREFIXED_NAME [:PREFIX "rdfs"] ":" "label"]] [:COLON "" ":" " "]
          [:LABEL "label"]]}
   {:exp [:LABEL_BLOCK
          "LABEL" [:SPACES " "]
          [:SUBJECT [:PREFIXED_NAME [:PREFIX "rdfs"] ":" "comment"]] [:COLON "" ":" " "]
          [:LABEL "comment"]]}
   {:exp [:LABEL_BLOCK
          "LABEL" [:SPACES " "]
          [:SUBJECT [:PREFIXED_NAME [:PREFIX "rdfs"] ":" "subClassOf"]] [:COLON "" ":" " "]
          [:LABEL "subclass of"]]}
   {:exp [:LABEL_BLOCK
          "LABEL" [:SPACES " "]
          [:SUBJECT [:IRIREF "<" "http://www.w3.org/TR/owl2-manchester-syntax/" ">"]] [:COLON "" ":" " "]
          [:LABEL "OMN"]]}
   {:exp [:DEFAULT_BLOCK
          "DEFAULT" [:SPACES " "]
          [:PREDICATE [:LABEL "label"]] [:SPACES " "]
          "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"]]}
   {:exp [:DEFAULT_BLOCK
          "DEFAULT" [:SPACES " "]
          [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
          "TYPE" [:SPACES " "] [:DATATYPE [:PREFIXED_NAME [:PREFIX "xsd"] ":" "string"]]]}
   {:exp [:SUBJECT_BLOCK [:SUBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "ontology"]]]}
   {:exp [:LITERAL_BLOCK [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "Example Ontology"]]}
   {:exp [:LINK_BLOCK
          [:PREDICATE [:LABEL "type"]] [:COLON_ARROW "" ":>" " "]
          [:OBJECT [:PREFIXED_NAME [:PREFIX "owl"] ":" "Ontology"]]]}
   {:exp [:SUBJECT_BLOCK
          [:SUBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "foo"]]]}
   {:exp [:LITERAL_BLOCK
          [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "Foo"]]}
   {:exp [:LINK_BLOCK
          [:PREDICATE [:LABEL "type"]] [:COLON_ARROW "" ":>" " "]
          [:OBJECT [:PREFIXED_NAME [:PREFIX "owl"] ":" "Class"]]]}
   {:exp [:LITERAL_BLOCK
          [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
          "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"] [:COLON "" ":" " "]
          [:LITERAL "A comment on 'Foo'."]]}
   {:exp [:ANNOTATION
          [:ARROWS ">" " "]
          [:LITERAL_BLOCK
           [:PREDICATE [:LABEL "comment"]] [:SPACES " "]
           "TYPE" [:SPACES " "] [:DATATYPE [:PREFIXED_NAME [:PREFIX "xsd"] ":" "string"]] [:COLON "" ":" " "]
           [:LITERAL "An annotation on a comment."]]]}
   {:exp [:ANNOTATION
          [:ARROWS ">>" " "]
          [:LITERAL_BLOCK
           [:PREDICATE [:LABEL "comment"]]
           [:SPACES " "] "TYPE" [:SPACES " "]
           [:DATATYPE [:PREFIXED_NAME [:PREFIX "xsd"] ":" "string"]][:COLON "" ":" " "]
           [:LITERAL "An annotation on an annotation."]]]}
   {:exp [:ANNOTATION
          [:ARROWS ">" " "]
          [:LINK_BLOCK
           [:PREDICATE [:PREFIXED_NAME [:PREFIX "rdfs"] ":" "seeAlso"]] [:COLON_ARROW "" ":>" " "]
           [:OBJECT [:PREFIXED_NAME [:PREFIX "ex"] ":" "bat"]]]]}
   {:exp [:LITERAL_BLOCK
          [:PREDICATE [:LABEL "comment"]] [:COLON "" ":" " "]
          [:LITERAL "Values can span multiple lines,\n  and include blank lines...\n\n  as long as each non-blank line is indented two spaces."]]}
   {:exp [:SUBJECT_BLOCK [:SUBJECT [:PREFIXED_NAME [:PREFIX "obo"] ":" "BFO_0000051"]]]}
   {:exp [:LITERAL_BLOCK [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "has part"]]}
   {:exp [:SUBJECT_BLOCK [:SUBJECT [:PREFIXED_NAME [:PREFIX "obo"] ":" "OBI_0000299"]]]}
   {:exp [:LITERAL_BLOCK [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "has_specified_output"]]}
   {:exp [:SUBJECT_BLOCK [:SUBJECT [:PREFIXED_NAME [:PREFIX "obo"] ":" "IAO_0000030"]]]}
   {:exp [:LITERAL_BLOCK [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "information content entity"]]}
   {:exp [:SUBJECT_BLOCK [:SUBJECT [:PREFIXED_NAME [:PREFIX "obo"] ":" "IAO_0000136"]]]}
   {:exp [:LITERAL_BLOCK [:PREDICATE [:LABEL "label"]] [:COLON "" ":" " "] [:LITERAL "is about"]]}])

#_(deftest test-environments
  (testing "the empty sequence of blocks return nil"
    (is (nil? (environments []))))
  (testing "a sequence of blocks gets decorated with an environment"
    (is (every?
         #(contains? % :env)
         (environments +test-blocks+))))
  (testing "if a starting environment is passed, that environment is added to each following element"
    (is (let [starting-env {:labels {:test-label "foo"}
                            :prefixes {:test-prefix "pref"}
                            :defaults {:test-default {"TYPE" "bar"}}
                            :graph "baz"
                            :subject "mumble"
                            :base "foobar"}
              result (environments +test-blocks+ starting-env)
              matches-start? (fn [res key] (= (get-in res (cons :env key)) (get-in starting-env key)))]
          (and (matches-start? (first result) [:subject])
               (matches-start? (first result) [:base])
               (every?
                #(and
                  (matches-start? % [:defaults :test-default])
                  (matches-start? % [:prefixes :test-label])
                  (matches-start? % [:prefixes :test-prefix]))
                result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; name expansion
#_(defspec test-<>
  (prop/for-all
   [v gen/string]
   (= (str "<" v ">") (<> v))))

#_(deftest test-expand-tree
  (testing "returns strings, keywords and numbers as-is"
    (is (= :test (expand-tree {} :test)))
    (is (= "test" (expand-tree {} "test")))
    (is (= 123 (expand-tree {} 123))))
  (testing "returns the expanded, absolute pointied names for IRIREFs, PREFIXED_NAMEs and LABELs"
    (is (= [:ABSOLUTE_IRI "<" "http://example.com/foo" ">"]
           (expand-tree
            {:labels {"foo" "http://example.com/foo"}}
            [:LABEL "foo"])))
    (is (= [:ABSOLUTE_IRI "<" "http://example.com/foo" ">"]
           (expand-tree
            {:prefixes {"ex" "http://example.com/"}}
            [:PREFIXED_NAME [:PREFIX "ex"] ":" "foo"])))
    (is (= [:ABSOLUTE_IRI "<" "http://example.com/foo" ">"]
           (expand-tree {:base "http://example.com/"} [:IRIREF "<" "foo" ">"])))
    (is (= [:ABSOLUTE_IRI "<" "http://example.com/foo" ">"]
           (expand-tree {} [:IRIREF "<" "http://example.com/foo" ">"]))))
  (testing "otherwise, recurs into the given syntax tree"
    (is (= [:PREDICATE [:ABSOLUTE_IRI "<" "http://example.com/foo" ">"]]
           (expand-tree
            {:prefixes {"ex" "http://example.com/"}}
            [:PREDICATE [:PREFIXED_NAME [:PREFIX "ex"] ":" "foo"]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; nquad generation
#_(deftest test-formatted
  (testing "double spaces are removed from all but the first line of multiline strings, and newlines are escaped"
    (is (= "\"foo\\nbar\\nbaz\"" (formatted "foo
  bar
  baz")))))

#_(deftest test-simple-block->nquad
  (testing "works as expected on LITERAL_BLOCKs"
    (is (= [nil "http://example.com/subject-3" "http://example.com/label" {:value "\"Relative IRI\""}]
           (simple-block->nquad
            {:exp [:LITERAL_BLOCK
                   [:PREDICATE [:IRIREF "<" "label" ">"]]
                   [:COLON "" ":" " "] [:LITERAL "Relative IRI"]],
             :env {:labels {"label" "http://www.w3.org/2000/01/rdf-schema#label"}
                   :subject "http://example.com/subject-3"
                   :base "http://example.com/"}}))))
  (testing "works as expected on LINK_BLOCKs"
    (is (= [nil "http://example.com/bar"
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            "http://www.w3.org/2002/07/owl#Class"]
           (simple-block->nquad
            {:exp [:LINK_BLOCK
                   [:PREDICATE [:LABEL "type"]]
                   [:COLON_ARROW "" ":>" " "]
                   [:OBJECT [:PREFIXED_NAME [:PREFIX "owl"] ":" "Class"]]],
             :env {:prefixes {"owl" "http://www.w3.org/2002/07/owl#"}
                   :labels {"type" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
                   :subject "http://example.com/bar"}})))))

#_(deftest test-annotation-block->nquads
  (testing "basic annotation generation"
    (is (= [[nil "_:b0" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://www.w3.org/2002/07/owl#Axiom"]
            [nil "_:b0" "http://www.w3.org/2002/07/owl#annotatedSource" "foo"]
            [nil "_:b0" "http://www.w3.org/2002/07/owl#annotatedProperty" "bar"]
            [nil "_:b0" "http://www.w3.org/2002/07/owl#annotatedTarget" "baz"]
            [nil "_:b0" "http://www.w3.org/2000/01/rdf-schema#seeAlso" "http://example.com/bat"]]
           (annotation-block->nquads
            0 [nil "foo" "bar" "baz"]
            {:exp [:ANNOTATION
                   [:ARROWS ">" " "]
                   [:LINK_BLOCK
                    [:PREDICATE
                     [:PREFIXED_NAME
                      [:PREFIX "rdfs"] ":" "seeAlso"]]
                    [:COLON_ARROW "" ":>" " "]
                    [:OBJECT
                     [:PREFIXED_NAME
                      [:PREFIX "ex"] ":" "bat"]]]],
             :env {:prefixes {"rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                              "ex" "http://example.com/"}}})))))

#_(deftest test-nquad-relevant-blocks
  (testing "drops any block types that don't affect the NQuads representation of a document"
    (is (= (map
            (fn [exp] {:exp exp})
            [[:LITERAL_BLOCK] [:LINK_BLOCK] [:ANNOTATION]])
           (nquad-relevant-blocks
            (map
             (fn [exp] {:exp exp})
             [[:PREFIX_BLOCK]
              [:LABEL_BLOCK]
              [:BASE_BLOCK]
              [:LITERAL_BLOCK]
              [:DEFAULT_BLOCK]
              [:SUBJECT_BLOCK]
              [:LINK_BLOCK]
              [:GRAPH_BLOCK]
              [:ANNOTATION]
              [:COMMENT_BLOCK]]))))))

#_(deftest test-find-target

  ;; NOTE - At the moment, blocks like
  ;;
  ;;   foo
  ;;   > bar
  ;;   >> baz
  ;;   > mumble
  ;;   >>> blarf
  ;;
  ;; are interpreted to mean that blarf is an annotation on mumble.
  ;; Is that the thing of least surprise, or should find-target actually
  ;; be looking for the first previous annotation with either 0 or (pred self)
  ;; arrow level?

  (testing "find the first entry in the annotation stack with a lower arrow count"
    (is (= :foo (find-target 1 [[1 :mumble] [2 :baz] [1 :bar] [0 :foo]])))
    (is (= :mumble (find-target 2 [[1 :mumble] [2 :baz] [1 :bar] [0 :foo]])))
    (is (= :baz (find-target 3 [[2 :baz] [1 :bar] [0 :foo]])))))

#_(deftest test-handle-annotation-block!
  ;; TODO
  )
(deftest test-handle-simple-block!
  ;; TODO
  )
#(deftest test-blocks->nquads
  ;; TODO
  )

(def an-nquad
  (gen/tuple
   gen/string-ascii gen/string-ascii gen/string-ascii
   (gen/one-of [gen/string-ascii (gen/return nil)])))

#_(defspec test-nquad->string
  (prop/for-all
   [q an-nquad]
   (let [[a b c d] q
         result (nquad->string q)]
     (and (util/ends-with? result ".\n")
          (util/substring? result a)
          (util/substring? result b)
          (util/substring? result c)
          (or (nil? d) (util/substring? result d))))))

#_(defspec test-print-nquads!
  (prop/for-all
   [v (gen/not-empty (gen/vector an-nquad))]
   (let [result (with-out-str (print-nquads! v))]
     (= (count v) (count (string/split-lines result))))))
