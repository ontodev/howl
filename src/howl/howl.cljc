(ns howl.howl
  "Convert HOWL to and from HOWL syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]

            [howl.link :as link]
            [howl.core :as core]
            [howl.util :as util]))

;; # BLOCKS

(def block-grammar
  (str
"<BLOCK> = WHITESPACE
          (COMMENT_BLOCK
           / PREFIX_BLOCK
           / LABEL_BLOCK
           / BASE_BLOCK
           / GRAPH_BLOCK
           / SUBJECT_BLOCK
           / STATEMENT_BLOCK)
          WHITESPACE

COMMENT_BLOCK   = #'#+.*'
PREFIX_BLOCK    = 'PREFIX' SPACES PREFIX COLON IRIREF
LABEL_BLOCK     = 'LABEL' SPACES LABEL DATATYPE COLON IRI
BASE_BLOCK      = 'BASE' SPACES IRIREF
GRAPH_BLOCK     = 'DEFAULT GRAPH' | 'GRAPH' (SPACES NAME)?
SUBJECT_BLOCK   = NAME_OR_BLANK
STATEMENT_BLOCK = ARROWS NAME DATATYPE COLON #'(\n|.)*.+'

WHITESPACE  = #'(\\r|\\n|\\s)*'
INDENTATION = #'(\\r|\\n|\\s)*  \\s*'
COLON       = #' *' ':'  #' +'
ARROWS      = #'>*' #'\\s*'"
   link/link-grammar))

(def block-parser (insta/parser block-grammar))

(defn parse-block
  [block]
  (let [result (block-parser block)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Parse failure")))
    (->> result
         (insta/transform link/link-transformations)
         vec)))


; To turn a string into a
; We have general functions for parsing block

(defmulti parse->block
  "Given an environment and a map with :parse-tree and :block-type keys,
   return a block map."
  (fn [env block] (:block-type block)))

(defmethod parse->block :default
  [env block]
  (println block)
  (throw (Exception. "Parse error")))

(defmulti block->parse
  "Given a block, return a new :parse-tree."
  :block-type)

; TODO: remove this
(defmethod block->parse :default
  [block]
  [:DEFAULT "DEFAULT"])

(defn block->howl-string
  [{:keys [leading-whitespace parse-tree trailing-whitespace]
    :or {leading-whitespace "LEAD"
         parse-tree [:ERROR "PARSE_TREE"]
         trailing-whitespace "TRAIL"}}]
  (str leading-whitespace
       (->> parse-tree flatten (filter string?) (apply str))
       trailing-whitespace))




;; ## COMMENT_BLOCK

; A COMMENT_BLOCK is a line with a hash (#) as the first character.
; The line is ignored by most processing,
; and does not have an NQuads representation.

; Example

[:COMMENT_BLOCK "# comment"]

(defmethod parse->block :COMMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  [env
   (assoc block :comment (last parse-tree))])

(defmethod block->parse :COMMENT_BLOCK
  [{:keys [comment] :as block}]
  [:COMMENT_BLOCK comment])


;; ## PREFIX_BLOCK

; Prefixes are used to contract and expand prefixed names.
; Prefix mappings are stored in the environment
; as forward- and reverse- maps.
;
; A PREFIX_BLOCK starts with a 'PREFIX' keyword,
; followed by a PREFIX and an IRI.
; The PREFIX_BLOCK does not have an NQuad representation.

; Example

[:PREFIX_BLOCK
 "PREFIX"
 [:SPACES " "]
 [:PREFIX "rdf"]
 [:COLON "" ":" " "]
 [:IRIREF "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]]

(defmethod parse->block :PREFIX_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [[_ _ _ [_ prefix] _ [_ _ iri _]] parse-tree]
    (link/check-iri iri)
    [(assoc-in env [:prefix-iri prefix] iri)
     (assoc block :prefix prefix :iri iri)]))


;; ## LABEL_BLOCK
;
; Labels provide mappings from human-readable strings to IRIs.
; They can also define default types for predicates.
; Label mappings are stored in the environment
; as forward- and reverse- maps.
; The DATATYPE definition is the same as STATEMENT_BLOCK below.

; Example

[:LABEL_BLOCK
 "LABEL"
 [:SPACES " "]
 [:LABEL "type"]
 [:DATATYPE " [" "LINK" "]"]
 [:COLON "" ":" " "]
 [:PREFIXED_NAME "rdf" ":" "type"]]

(defmethod parse->block :LABEL_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [[_ _ _ [_ label] dt _ id] parse-tree
        iri (link/id->iri env id)
        datatype (link/unpack-datatype env (get dt 2))]
    [(assoc-in
      env
      [:labels label]
      (merge {:iri iri} (when datatype {:datatype datatype})))
     (assoc block :label label :iri iri :datatype datatype)]))



;; ## BASE_BLOCK
;
; The base block sets the base IRI
; use to resolve relate IRIs.

; Example

[:BASE_BLOCK "BASE" [:SPACES " "] [:IRIREF "<" "http://example.com" ">"]]

(defmethod parse->block :BASE_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [[_ _ _ [_ _ iri _]] parse-tree]
    (link/check-iri iri)
    [(assoc env :base iri)
     (assoc block :base iri)]))



;; ## GRAPH_BLOCK
;
; The graph block declares the graph for all statements
; until the next graph block.
; Processing starts with the (unnamed) default graph.
; Graph blocks can either be 'GRAPH NAME'
; or 'DEFAULT GRAPH'.

; Examples
[:GRAPH_BLOCK "DEFAULT GRAPH"]
[:GRAPH_BLOCK "GRAPH" [:SPACES " "] [:PREFIXED_NAME "ex" ":" "graph"]]

(defmethod parse->block :GRAPH_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [graph-name (get parse-tree 3)
        graph      (when graph-name (link/name->iri env graph-name))]
    [(assoc env :graph graph)
     (assoc
      block
      :graph-name graph-name
      :graph graph)]))


;; ## SUBJECT_BLOCK
;
; The subject block declares the subject of all statements
; until the next subject block.

; Example

[:SUBJECT_BLOCK [:PREFIXED_NAME "ex" ":" "subject"]]

(defmethod parse->block :SUBJECT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [subject-name (second parse-tree)
        subject      (link/name->iri env subject-name)]
    [(assoc env :subject subject)
     (assoc
      block
      :subject-name subject-name
      :subject subject)]))



;; ## STATEMENT_BLOCK

; Example

[:STATEMENT_BLOCK
 [:ARROWS "" ""]
 [:LABEL "type"]
 [:DATATYPE " [" "LINK" "]"]
 [:COLON " " ":" " "]
 "owl:Class"]

(defmethod parse->block :STATEMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [predicate-name  (get parse-tree 2)
        predicate-label (link/name->label predicate-name)
        predicate       (link/name->iri env predicate-name)
        datatype-name   (get-in parse-tree [3 2])
        datatype
        (or (when datatype-name (link/unpack-datatype env datatype-name))
            (when predicate-label
              (get-in env [:labels predicate-label :datatype])))
        [object content] (core/content->parse env datatype (last parse-tree))]

    [env
     (assoc
      block
      :parse-tree (assoc parse-tree 5 content) ; update parse
      :arrows (get-in parse-tree [1 1])
      :predicate-name predicate-name
      :datatype-name datatype-name
      :content content
      :graph (:graph env)
      :subject (:subject env) ; TODO: handle missing
      :predicate predicate
      :object object
      :datatype datatype)]))


(defn group-lines
  "Given a sequence of lines, returns a lazy sequence of grouped lines"
  [lines]
  (partition-by
   (let [ct (volatile! 0)]
     #(do (when (not (or (string/blank? %) (util/starts-with? % "  ")))
            (vswap! ct inc))
          @ct))
   lines))

(defn process-block
  "Given an environment a source, a line number, and a block string,
   return the update environment and the block map."
  [{:keys [source line] :as env} source-string]
  (let [[[_ lws] parse-tree [_ tws]] (parse-block source-string)]
    (parse->block
     env
     {:source source
      :line line
      :string source-string
      :block-type (first parse-tree)
      :parse-tree parse-tree
      :leading-whitespace lws
      :trailing-whitespace tws})))

(defn lines->blocks
  "Given a sequence of lines,
   group them, process them,
   and return a lazy sequence of block maps."
  [{:keys [source] :or {source "interactive"} :as env} lines]
  (reduce
   (fn [env lines]
     (let [[env block] (process-block env (apply str lines))]
       (-> env
           (update-in [:blocks] (fnil conj []) block)
           (update-in [:line] + (count lines)))))
   (assoc env :source source :line 1)
   (group-lines lines)))


