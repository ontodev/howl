(ns howl.doc-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [howl.util :as util]
            [howl.link :as link]
            [howl.howl :as howl]
            [howl.json :as json]
            howl.manchester))

;; Parse the doc/design.md file
;; looking for JSON blocks,
;; then test each block.

(def env
  {:source "example.howl"
   :line 1
   :prefix-iri
   {"rdf"  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
    "xsd"  "http://www.w3.org/2001/XMLSchema#"
    "owl"  "http://www.w3.org/2002/07/owl#"
    "ex"   "http://example.com/"}
   :labels
   {"comment"
    {:iri "http://www.w3.org/2000/01/rdf-schema#comment"}
    "foo"
    {:iri "http://example.com/foo"}
    "has part"
    {:iri "http://purl.obolibrary.org/obo/BFO_0000050"}
    "Manchester"
    {:iri "http://www.w3.org/TR/owl2-manchester-syntax/"}
    "subclass of"
    {:iri "http://www.w3.org/2000/01/rdf-schema#subClassOf"
     :datatypes ["LINK" "http://www.w3.org/TR/owl2-manchester-syntax/"]}
    "type"
    {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
     :datatypes ["LINK"]}}
   :iri-label
   {"http://www.w3.org/TR/owl2-manchester-syntax/" "Manchester"}
   :iri-prefix
   {"http://www.w3.org/2001/XMLSchema#" "xsd"}
   :graph "http://example.com/current-graph"
   :subject "http://example.com/current-subject"
   :statement-stack
   [["http://example.com/current-subject"
     "http://example.com/previous-predicate"
     "http://example.com/previous-object"
     "http://example.com/previous-datatype"]]})

(defn fix
  "Fix a block to make it read for comparison."
  [{:keys [subject] :as block}]
  (if (link/blank? subject)
    (assoc block :subject "_:b1")
    block))

(defn run-test
  "Given a block map, process the :block value,
   and check that the result is the same as the original block."
  [block]
  (try
    (testing block
      (is (= (fix (howl/process-block env (:string block)))
             block)))
    (catch Exception e
      (throw (Exception. (str "Failed while parsing block: " (.getMessage e)))))))

;; Find all indented JSON blocks in the doc/design.md.
(deftest test-design-examples
  (->> "doc/design.md"
       slurp
       string/split-lines
       ;(drop-while #(not (.startsWith % "## Syntax and Parsing")))
       (partition-by #(.startsWith % "    "))
       (filter #(.startsWith (first %) "    ")) ; keep indented
       (map (fn [lines] (map #(string/replace % #"^    " "") lines)))
       (map (fn [lines] (str (string/join "\n" lines) "\n")))
       (filter #(.startsWith % "{"))
       ;(take 5)
       (map json/json->block)
       (map run-test)
       doall
       (apply = true)))

(deftest test-happy-blocks
  (->> "test/happy_blocks"
       io/file
       file-seq
       (map #(.getPath %))
       (filter string?)
       (filter #(util/ends-with? % ".json"))
       (map slurp)
       (map json/json->block)
       (map run-test)
       doall
       (apply = true)))