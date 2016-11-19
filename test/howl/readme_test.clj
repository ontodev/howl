(ns howl.readme-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [howl.howl :as howl]
            [howl.json :as json]))

;; Parse the README.md file
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
    "type"
    {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
     :datatype "LINK"}}
   :current-graph-iri "http://example.com/current-graph"
   :current-subject-iri "http://example.com/current-subject"})

(defn run-test
  "Given a block map, process the :block value,
   and check that the result is the same as the original block."
  [block]
  (try
    (testing block
      (is (= (second (howl/process-block env (:string block)))
             block)))
    (catch Exception e
      (throw (Exception. (str "Failed while parsing block: " (.getMessage e)))))))

;; Find all indented JSON blocks in the README.
(deftest test-readme-examples
  (->> "README.md"
       slurp
       string/split-lines
       (drop-while #(not (.startsWith % "## Syntax and Parsing")))
       (partition-by #(.startsWith % "    "))
       (filter #(.startsWith (first %) "    ")) ; keep indented
       (map (fn [lines] (map #(string/replace % #"^    " "") lines)))
       (map (fn [lines] (str (string/join "\n" lines) "\n")))
       (filter #(.startsWith % "{"))
       ;(take 2)
       (map json/json->block)
       (map run-test)
       doall
       ;(#(do (println "Running" (count %) "tests on README") %))
       (apply = true)))
