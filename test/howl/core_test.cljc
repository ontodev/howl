(ns howl.core-test
  "Test core functions."
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test
             :refer :all :include-macros true]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties
             :as prop :include-macros true]

            [clojure.string :as string]
            [instaparse.core :as insta]

            [howl.util :as util]
            [howl.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing
(deftest test-group-lines
  (testing "creates singleton groups from standalone lines"
    (is (= '(("foo") ("bar") ("baz"))
           (group-lines ["foo" "bar" "baz"]))))

  (testing "groups consecutive lines that start with two spaces"
    (is (= '(("foo" "  bar") ("baz"))
           (group-lines ["foo" "  bar" "baz"])))))

(deftest test-top-level-productions
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

(deftest test-labels-parse
 (testing "Bad labels either cause an outright parse failure, or return a parse tree that does not designate a :LABEL_BLOCK"
   (doseq [bad-label bad-labels]
     (is (or (insta/failure? (block-parser bad-label))
             (not= :LABEL_BLOCK (first (block-parser bad-label))))))))

(deftest test-lines->blocks
  (testing "returns a {:exp ParseTree} map, with source and line count in metadata"
    (is (= (first (lines->blocks ["PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"]))
           {:exp [:PREFIX_BLOCK "PREFIX"
                  [:SPACES " "] [:PREFIX "rdf"] [:COLON_ARROW "" ":>" " "]
                  [:PREFIXED
                   [:IRIREF "<" "h" "t" "t" "p" ":" "/" "/" "w" "w" "w" "." "w" "3" "." "o" "r" "g" "/" "1" "9" "9" "9" "/" "0" "2" "/" "2" "2" "-" "r" "d" "f" "-" "s" "y" "n" "t" "a" "x" "-" "n" "s" "#" ">"]]]}))
    (is (= (meta (first (lines->blocks ["PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"])))
           {:origin {:name "interactive", :line 1}}))))

(deftest test-parse-tree->string
  (testing "returns a string unmodified"
    (is (= "foo" (parse-tree->string "foo"))))

  (testing "for :LABEL, :PREFIX, :TRAILING_WHITESPACE, :SPACES and :ABSOLUTE_IRI blocks, returns the second element"
    (is (= "foo" (parse-tree->string [:LABEL "foo"])))
    (is (= "foo" (parse-tree->string [:PREFIX "foo"])))
    (is (= "foo" (parse-tree->string [:TRAILING_WHITESPACE "foo"])))
    (is (= "foo" (parse-tree->string [:SPACES "foo"])))
    (is (= "foo" (parse-tree->string [:ABSOLUTE_IRI "foo"]))))

  (testing "for other forms, joins the recursion on rest"
    (is (= "foobar" (parse-tree->string [:BLAH [:LABEL "foo"] "bar"])))
    (is (= "foo bar" (parse-tree->string [:BLAH [:LABEL "foo"] [:SPACES " "] [:BLEEH [:BLUH "bar"]]])))))

(deftest test-lines<->blocks
  (testing "calling block->string on an element of the return value of lines->blocks returns the string we started with"
    (is (let [ln "PREFIX rdf:> <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"]
          (= ln (block->string (first (lines->blocks [ln]))))))))

(deftest test-condense-tree
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

(deftest test-parse-expressions
  ;; TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Environment extraction
(deftest test-name-from-node
  (testing "get the name out of a blank node"
    (is (= "testing"
           (name-from-node
            {} [:BLANK_NODE "_:" "testing"]))))
  (testing "get the IRI out of an absolute IRI"
    (is (= "http://example.com/"
           (name-from-node
            {} [:ABSOLUTE_IRI "http://example.com/"]))))
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
            {} [:SUBJECT [:ABSOLUTE_IRI "http://example.com/"]])))))

(deftest test-locate
  (testing "when there is an :origin property in the targets meta, return it"
    (is (= (locate ^{:origin {:name "local" :line 3}} [:BLOCK])
           {:name "local" :line 3})))
  (testing "when there is no :origin in the targets meta, return the stringified target"
    (is (= (locate [:BLOCK]) (str [:BLOCK])))))

(deftest test-parse-tree->names
  (testing "a :PREFIX_BLOCK returns new prefixes"
    (is (= {:prefixes {"ex" "http://example.com/"}}
           (parse-tree->names
            {} [:PREFIX_BLOCK
                "PREFIX" [:SPACES " "] [:PREFIX "ex"] [:COLON_ARROW "" ":>" " "]
                [:PREFIXED [:IRIREF "<" "http://example.com/" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :LABEL_BLOCK returns new labels"
    (is (= {:labels {"label" "foo"}}
           (parse-tree->names
            {} [:LABEL_BLOCK
                "LABEL" [:SPACES " "] [:SUBJECT [:IRIREF "<" "foo" ">"]] [:COLON "" ":" " "]
                [:LABEL "label"]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :DEFAULT_BLOCK can return new default values for TYPE"
    (is (= {:defaults {"foo" {"TYPE" "bar"}}}
           (parse-tree->names
            {} [:DEFAULT_BLOCK
                "DEFAULT" [:SPACES " "] [:PREDICATE [:IRIREF "<" "foo" ">"]] [:SPACES " "]
                "TYPE" [:SPACES " "] [:DATATYPE [:IRIREF "<" "bar" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :DEFAULT_BLOCK can return a new default value for LANUGAGE"
    (is (= {:defaults {"foo" {"LANGUAGE" "en"}}}
           (parse-tree->names
            {} [:DEFAULT_BLOCK
                "DEFAULT" [:SPACES " "] [:PREDICATE [:IRIREF "<" "foo" ">"]] [:SPACES " "]
                "LANGUAGE" [:SPACES " "] [:LANGUAGE "en"]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :SUBJECT_BLOCK returns a new :subject"
    (is (= {:subject "foo"}
           (parse-tree->names
            {} [:SUBJECT_BLOCK
                [:SUBJECT [:IRIREF "<" "foo" ">"]]
                [:TRAILING_WHITESPACE ""]]))))
  (testing "a :BASE_BLOCK returns a new :base"
    (is (= {:base "http://example.com/"}
           (parse-tree->names
            {} [:BASE_BLOCK
                "BASE" [:SPACES " "]
                [:BASE [:IRIREF "<" "http://example.com/" ">"]]
                [:TRAILING_WHITESPACE "\n"]]))))
  (testing "a :GRAPH_BLOCK returns a new :graph and a new :subject (both the given graph)"
    (is (= {:graph "baz", :subject "baz"}
           (parse-tree->names
            {} [:GRAPH_BLOCK
                "GRAPH" [:SPACES " "]
                [:GRAPH [:IRIREF "<" "baz" ">"]]
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

(deftest test-merge-environments
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

(deftest test-environments
  ;; TODO - 1. that it works
  ;;        2. that it includes starting data when provided
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; name expansion

(deftest test-expand-tree
  ;; TODO - 1. does nothing for keywords, strings or numbers
  ;;        2. expands IRIREF and PREFIXED_NAME into ABSOLUTE_URI
  ;;        3. errors when no such name in env
  ;;        4. recurs otherwise
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; nquad generation
(deftest test-formatted
  (testing "quoted and angled strings are returned"
    (is (= "<http://example.com/>" (formatted "<http://example.com/>")))
    (is (= "\"foo\"" (formatted "\"foo\""))))
  (testing "un-angled strings starting with http are angled"
    (is (= "<http://example.com/>" (formatted "http://example.com/"))))
  (testing "other strings are quoted"
    (is (= "\"foo\"" (formatted "foo"))))
  (testing "double spaces are removed from all but the first line of multiline strings, and newlines are escaped"
    (is (= "\"foo\\nbar\\nbaz\"" (formatted "foo
  bar
  baz")))))

(deftest test-simple-block->nquad)
(deftest test-annotation-block->nquads)
(deftest test-nquad-relevant-blocks)
(deftest test-find-target)
(deftest test-handle-annotation-block!)
(deftest test-handle-simple-block!)
(deftest test-blocks->nquads)

(def an-nquad
  (gen/tuple
   gen/string-ascii gen/string-ascii gen/string-ascii
   (gen/one-of [gen/string-ascii (gen/return nil)])))

(defspec test-nquad->string
  (prop/for-all
   [q an-nquad]
   (let [[a b c d] q
         result (nquad->string q)]
     (and (util/ends-with? result ".\n")
          (util/includes? result a)
          (util/includes? result b)
          (util/includes? result c)
          (or (nil? d) (util/includes? result d))))))

(defspec test-print-nquads!
  (prop/for-all
   [v (gen/not-empty (gen/vector an-nquad))]
   (let [result (with-out-str (print-nquads! v))]
     (= (count v) (count (string/split-lines result))))))
