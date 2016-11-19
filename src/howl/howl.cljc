(ns howl.howl
  "Convert HOWL to and from HOWL syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]

            [howl.link :as link]
            [howl.util :as util]))

;; # BLOCKS

(def block-grammar
  (str
"<BLOCK> = WHITESPACE
          (COMMENT_BLOCK
           / PREFIXES_BLOCK
           / LABELS_BLOCK
           / BASE_BLOCK
           / GRAPH_BLOCK
           / SUBJECT_BLOCK
           / STATEMENT_BLOCK)
          WHITESPACE

COMMENT_BLOCK = #'#+\\s*' #'.*'
PREFIXES_BLOCK = 'PREFIXES' (INDENTATION PREFIX_LINE)+
PREFIX_LINE = PREFIX COLON IRIREF
LABELS_BLOCK = 'LABELS' (INDENTATION LABEL_LINE)+
LABEL_LINE = LABEL DATATYPE COLON IRI
BASE_BLOCK = 'BASE' SPACES IRIREF
GRAPH_BLOCK = 'DEFAULT GRAPH' | 'GRAPH' (SPACES NAME)?
SUBJECT_BLOCK = NAME_OR_BLANK
STATEMENT_BLOCK = ARROWS NAME DATATYPE COLON #'(\n|.)*.+'

WHITESPACE  = #'(\\r|\\n|\\s)*'
INDENTATION = #'(\\r|\\n|\\s)*  \\s*'
COLON       = #' *' ':'  #' +'
ARROWS   = #'>*' #'\\s*'"
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

(defmethod parse->block :COMMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  [env
   (assoc
    block
    :hash    (second parse-tree)
    :comment (last parse-tree))])

(defmethod block->parse :COMMENT_BLOCK
  [{:keys [hash comment] :as block}]
  [:COMMENT_BLOCK hash comment])


;; ## PREFIXES_BLOCK

; Prefixes are used to contract and expand prefixed names.
; Prefix mappings are stored in the environment
; as forward- and reverse- maps.
;
; A PREFIXES_BLOCK starts with a 'PREFIXES' keyword,
; followed by one or or more PREFIX_LINES
; each containing a PREFIX and and IRI.
; The PREFIXES_BLOCK does not have an NQuad representation.

(defn extract-prefixes
  "Given a parse tree for PREFIXES_BLOCK,
   return a map from prefix string to IRI string."
  [parse-tree]
  (->> parse-tree
       (filter vector?)
       (filter #(= :PREFIX_LINE (first %)))
       (map
        (fn [[_ [_ prefix] _ [_ _ iri _]]]
          [prefix (link/check-iri iri)]))
       (into {})))

(defmethod parse->block :PREFIXES_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [prefixes (extract-prefixes parse-tree)]
    [(update-in env [:prefix-iri] merge prefixes)
     (assoc block :prefixes prefixes)]))


;; ## LABELS_BLOCK
;
; Labels provide mappings from human-readable strings to IRIs.
; They can also define default types for predicates.
; Label mappings are stored in the environment
; as forward- and reverse- maps.
; The DATATYPE definition is the same as STATEMENT_BLOCK below.

(defn extract-label
  [env datatype id]
  (let [datatype (link/unpack-datatype env datatype)]
    (merge
     {:iri (link/id->iri env id)}
     (when datatype {:datatype datatype}))))

(defn extract-labels
  "Given a parse tree for LABELS_BLOCK,
   return a map from label string to map."
  [env parse-tree]
  (reduce
   (fn [labels [_ [_ label] datatype _ id]]
     (assoc
      labels
      label
      (extract-label
       (update-in env [:labels] merge labels)
       (get datatype 2)
       id)))
   {}
   (->> parse-tree
        (filter vector?)
        (filter #(= :LABEL_LINE (first %))))))

(defmethod parse->block :LABELS_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [labels (extract-labels env parse-tree)]
    [(update-in env [:labels] merge labels)
     (assoc block :labels labels)]))



;; ## BASE_BLOCK
;
; The base block sets the base IRI
; use to resolve relate IRIs.

(defmethod parse->block :BASE_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [base-iri (link/check-iri (get-in parse-tree [3 2]))]
    ; TODO: check that IRI is absolute
    [(assoc env :base-iri base-iri)
     (assoc block :base-iri base-iri)]))



;; ## GRAPH_BLOCK
;
; The graph block declares the graph for all statements
; until the next graph block.
; Processing starts with the (unnamed) default graph.
; Graph blocks can either be 'GRAPH NAME'
; or 'DEFAULT GRAPH'.

(defmethod parse->block :GRAPH_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [graph     (get parse-tree 3 nil)
        graph-iri (when graph (link/name->iri env graph))]
    ; TODO: check that IRI is absolute
    [(assoc env :current-graph-iri graph-iri)
     (assoc
      block
      :graph graph
      :graph-iri graph-iri)]))


;; ## SUBJECT_BLOCK
;
; The subject block declares the subject of all statements
; until the next subject block.

(defmethod parse->block :SUBJECT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [subject     (second parse-tree)
        subject-iri (link/name->iri env subject)]
    ; TODO: check that IRI is absolute
    [(assoc env :current-subject-iri subject-iri)
     (assoc
      block
      :subject subject
      :subject-iri subject-iri)]))



;; ## STATEMENT_BLOCK

(defmulti content->parse
  "Given an environment,
   a datatype IRI string (or nil when the object is an IRI),
   and a content string,
   return the object and the parse tree for the content."
  (fn [env datatype-iri content] datatype-iri))

; The default behaviour is to remove indentation.

(defmethod content->parse :default
  [env datatype-iri content]
  (let [unindented (string/replace content #"(?m)^  |^ " "")]
    [unindented unindented]))

(defmethod content->parse "LINK"
  [env datatype-iri content]
  (let [result (link/parse-link content)]
    [(link/name->iri env result) result]))

(defmethod parse->block :STATEMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [predicate     (get parse-tree 2)
        predicate-iri (link/name->iri env predicate)
        datatype      (get-in parse-tree [3 2])
        datatype-iri
        (or (when datatype (link/unpack-datatype env datatype))
            (when (= :LABEL (first predicate))
              (get-in env [:labels (second predicate) :datatype])))
        [object content] (content->parse env datatype-iri (last parse-tree))]
    [env
     (assoc
      block
      :parse-tree (assoc parse-tree 5 content) ; update parse
      :arrows (get-in parse-tree [1 1])
      :predicate predicate
      :datatype datatype
      :content content
      :graph-iri (:current-graph-iri env)
      :subject-iri (:current-subject-iri env) ; TODO: handle missing
      :predicate-iri predicate-iri
      :object object
      :datatype-iri datatype-iri)]))


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


