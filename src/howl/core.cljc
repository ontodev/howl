(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]
            [cemerick.url :refer [url]]

            [howl.util :as util :refer [<>> owl> rdf> rdf-schema>]]
            [howl.expression :as exp]))

;; ## Links

; Adapted from the NQuads spec *)
; WARN: Java doesn't support five-digit Unicode for \u10000-\uEFFFF
(def iri-grammar "
IRIREF           = '<' (#'[^\u0000-\u0020<>\"{}|^`\\\\]' | UCHAR)* '>'
BLANK_NODE_LABEL = '_:' (PN_CHARS_U | #'[0-9]') ((PN_CHARS | '.')* PN_CHARS)?
UCHAR            = '\\\\u' HEX HEX HEX HEX | '\\\\U' HEX HEX HEX HEX HEX HEX HEX HEX
PN_CHARS_BASE    = #'[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]'
PN_CHARS_U       = PN_CHARS_BASE | '_' | ':'
PN_CHARS         = PN_CHARS_U | #'[-0-9\u00B7\u0300-\u036F\u203F-\u2040]'
HEX              = #'[0-9A-Fa-f]+'
")

(def prefixed-name-grammar "
PREFIXED_NAME = #'(\\w|-)+' ':' #'[^\\s:/][^\\s:\\]]*'
PREFIX        = #'(\\w|-)+'
")

(def label-grammar "
LABEL   = !(KEYWORD | '<' | '>' | '[' | ']' | '#') (WORD SPACES?)* WORD
KEYWORD = 'BASE' | 'GRAPH' | 'PREFIXES' | 'LABELS' | 'DEFAULT'
<WORD>  = #'[^\\s]*[^:\\]\\s]'
SPACES  = #' +'
")

(def link-grammar-partial
  "LINK = NAME_OR_BLANK
<NAME_OR_BLANK> = IRIREF / BLANK_NODE_LABEL / PREFIXED_NAME / LABEL
<NAME> = IRIREF / PREFIXED_NAME / LABEL
<IRI>  = IRIREF / PREFIXED_NAME")

(def link-transformations
  {:IRIREF (fn [& xs] [:IRIREF "<" (apply str (rest (butlast xs))) ">"])
   :LABEL  (fn [& xs] [:LABEL (apply str xs)])})

(def link-grammar
  (string/join
   \newline
   [link-grammar-partial
    iri-grammar
    prefixed-name-grammar
    label-grammar]))

(def link-parser (insta/parser link-grammar))

(defn parse-link
  [content]
  (let [result (link-parser content)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Link parser failure")))
    (->> result
         (insta/transform link-transformations)
         second)))

(defn check-iri
  [iri]
  (str (url iri)))

(defn resolve-iri
  [{:keys [base-iri] :or {base-iri "file:///howl-unspecified-base/"}} iri]
  (try
   (check-iri iri)
   (catch Exception e
     (str (url base-iri iri)))))

(defn iriref->iri
  [env [_ _ iri _]]
  (resolve-iri env iri))

(defn prefixed-name->iri
  [env [_ prefix _ local]]
  (when-not (get-in env [:prefix-iri prefix])
    (throw (Exception. (format "Could not find prefix '%s'." prefix))))
  (resolve-iri env (str (get-in env [:prefix-iri prefix]) local)))

(defn label->iri
  "Given a label, return an absolute IRI string."
  [env [_ label]]
  (when-not (get-in env [:label-iri label])
    (throw (Exception. (format "Could not find label '%s'." label))))
  ; Should be absolute when assigned.
  (get-in env [:label-iri label]))

(defn id->iri
  [env parse]
  (case (first parse)
    :IRIREF        (iriref->iri env parse)
    :PREFIXED_NAME (prefixed-name->iri env parse)
    ; TODO: catch error
    ))

(defn name->iri
  [env parse]
  (case (first parse)
    :IRIREF        (iriref->iri env parse)
    :PREFIXED_NAME (prefixed-name->iri env parse)
    :LABEL         (label->iri env parse)
    ; TODO: catch error
    ))

(def block-grammar-partial "
<BLOCK> = WHITESPACE
          (COMMENT_BLOCK
           / PREFIXES_BLOCK
           / LABELS_BLOCK
           / BASE_BLOCK
           / GRAPH_BLOCK
           / SUBJECT_BLOCK
           / STATEMENT_BLOCK)
          WHITESPACE

COLON       = #' *' ':'  #' +'
WHITESPACE  = #'(\\r|\\n|\\s)*'
INDENTATION = #'(\\r|\\n|\\s)*  \\s*'
")


;; # BLOCKS

; There are seven block types in HOWL:
; COMMENT, PREFIXES, LABELS, BASE, GRAPH, SUBJECT, STATEMENT
;
; The core of HOWL processing is the "block map",
; which contains enough information to convert into any supported format:
; HOWL, NQuads, JSON.
;
; We use a hub-and-spoke model with the block map representation in the middle,
; then provide methods for each conversion:
;
; - HOWL string to block
; - block to HOWL string
; - NQuad to block
; - block to NQuad
; - JSON string to block
; - block to JSON string
;
; We also have blocks to blocks transformations,
; such as normalization and sorting.


; To turn a string into a
; We have general functions for parsing block

(defmulti parse->block
  "Given an environment and a map with :parse-tree and :block-type keys,
   return the updated environment and block map."
  (fn [env block] (:block-type block)))

(defmethod parse->block :default
  [env block]
  (println block)
  (throw (Exception. "Parse error")))

; TODO: block->string



; Sometimes we want to reformat or normalize a block.
; By default, we do nothing.

(defmulti normalize-block
  "Given an environment and a block map,
   return the normalized block map."
  (fn [env block] (:block-type block)))

(defmethod normalize-block :default
  [env block]
  block)


; When converting blocks to NQuads
; Be default, do nothing.

(defmulti block->nquads
  "Given a (fully expanded) block,
   return a sequence of zero or more NQuad vectors."
  :block-type)

(defmethod block->nquads :default
  [block]
  [])




;; ## COMMENT_BLOCK

; A COMMENT_BLOCK is a line with a hash (#) as the first character.
; The line is ignored by most processing,
; and does not have an NQuads representation.

(def comment-block-grammar "COMMENT_BLOCK = #'#+\\s*' #'.*'")

(defmethod parse->block :COMMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  [env
   (assoc
    block
    :hash    (second parse-tree)
    :comment (nth parse-tree 2))])




;; ## PREFIXES_BLOCK

; Prefixes are used to contract and expand prefixed names.
; Prefix mappings are stored in the environment
; as forward- and reverse- maps.
;
; A PREFIXES_BLOCK starts with a 'PREFIXES' keyword,
; followed by one or or more PREFIX_LINES
; each containing a PREFIX and and IRI.
; The PREFIXES_BLOCK does not have an NQuad representation.

(def prefixes-block-grammar
  "PREFIXES_BLOCK = 'PREFIXES' (INDENTATION PREFIX_LINE)+
PREFIX_LINE = PREFIX COLON IRIREF")

(defn extract-prefixes
  "Given a parse tree for PREFIXES_BLOCK,
   return a map from prefix string to IRI string."
  [parse-tree]
  (->> parse-tree
       (filter vector?)
       (filter #(= :PREFIX_LINE (first %)))
       (map
        (fn [[_ [_ prefix] _ [_ _ iri _]]]
          ; TODO: make sure IRI is absolute?
          [prefix (check-iri iri)]))
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

(def labels-block-grammar
"LABELS_BLOCK = 'LABELS' (INDENTATION LABEL_LINE)+
LABEL_LINE = LABEL COLON IRI") ; TODO: add DATATYPE

(defn extract-labels
  "Given a parse tree for LABELS_BLOCK,
   return a map from label string to IRI string."
  [env parse-tree]
  (->> parse-tree
       (filter vector?)
       (filter #(= :LABEL_LINE (first %)))
       (map
        (fn [[_ [_ label] _ id]]
          [label (id->iri env id)]))
       (into {})))

(defmethod parse->block :LABELS_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [labels (extract-labels env parse-tree)]
    [(update-in env [:label-iri] merge labels)
     (assoc block :labels labels)]))



;; ## BASE_BLOCK
;
; The base block sets the base IRI
; use to resolve relate IRIs.

(def base-block-grammar "BASE_BLOCK = 'BASE' SPACES IRIREF")

(defmethod parse->block :BASE_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [base-iri (check-iri (get-in parse-tree [3 2]))]
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

(def graph-block-grammar "GRAPH_BLOCK = 'DEFAULT GRAPH' | 'GRAPH' (SPACES NAME)?")

(defmethod parse->block :GRAPH_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [graph     (get parse-tree 3 nil)
        graph-iri (when graph (name->iri env graph))]
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

(def subject-block-grammar "SUBJECT_BLOCK = NAME_OR_BLANK")

(defmethod parse->block :SUBJECT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [subject     (second parse-tree)
        subject-iri (name->iri env subject)]
    ; TODO: check that IRI is absolute
    [(assoc env :current-subject-iri subject-iri)
     (assoc
      block
      :subject subject
      :subject-iri subject-iri)]))



;; ## STATEMENT_BLOCK

(def statement-block-grammar
  "STATEMENT_BLOCK = ARROWS NAME DATATYPE COLON #'(\n|.)*.+'
ARROWS   = #'>*' #'\\s*'
DATATYPE = '' | #' +\\[' ('LINK' | LANGUAGE | NAME) ']'
LANGUAGE = #'@[a-zA-Z]+(-[a-zA-Z0-9]+)*'")

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

(defmethod content->parse nil
  [env datatype-iri content]
  (let [result (parse-link content)]
    [(name->iri env result) result]))

(def rdf:PlainLiteral "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")

(defn get-datatype
  "Given an environment, a predicate parse, and a datatype parse,
   return the datatype-iri (nil for object IRI) and language (or nil).
   This will be the for the given datatype (unless it is null),
   or the predicate's default datatype (if it has one),
   or just rdf:PlainLiteral."
  [env predicate datatype]
  (cond
   (= "LINK" datatype)
   [nil nil]

   (= :LANGUAGE (first datatype))
   [rdf:PlainLiteral (second datatype)]

   datatype
   [(name->iri env datatype) nil]

   (and (= :LABEL (first predicate))
        (get-in env [:label-datatype (second predicate)]))
   (get-in env [:label-datatype (second predicate)])

   :else
   [rdf:PlainLiteral nil]))

(defmethod parse->block :STATEMENT_BLOCK
  [env {:keys [parse-tree] :as block}]
  (let [predicate     (get parse-tree 2)
        predicate-iri (name->iri env predicate)
        datatype      (get-in parse-tree [3 2])
        [datatype-iri language] (get-datatype env predicate datatype)
        [object content] (content->parse env datatype-iri (last parse-tree))]
    ; TODO: check that IRIs are absolute
    [env ; TODO: add rdfs:label
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
      :datatype-iri datatype-iri
      :language language)]))

(defmethod block->nquads :STATEMENT_BLOCK
  [{:keys [graph-iri subject-iri predicate-iri object datatype-iri language]}]
  [[graph-iri
    subject-iri
    predicate-iri
    {:value (-> object
                (string/replace "\n" "\\n")
                (string/replace "\"" "\\\""))
     :datatype datatype-iri
     :language language}]])


(def block-grammar
  (string/join
   \newline
   [block-grammar-partial
    comment-block-grammar
    prefixes-block-grammar
    labels-block-grammar
    base-block-grammar
    graph-block-grammar
    subject-block-grammar
    statement-block-grammar
    link-grammar]))

(def block-parser (insta/parser block-grammar))

(defn parse-block
  [block]
  (let [result (block-parser block)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Parse failure")))
    (->> result
         (insta/transform link-transformations)
         vec)))

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

(defn reset-environment
  [env]
  (dissoc env :current-graph-iri :current-subject-iri))


;; ## NQUADS
;
; NQuads is a line-based concrete syntax for RDF.
; Each line consists of a subject, predicate, object, and optional graph.
; The predicate and graph are always absolute IRIs.
; The subject is an absolute IRI or blank node.
; The object can be: an IRI, a blank node, or a literal.
; Literals are quoted strings with an optional language tag or type IRI.
;
; We represent NQuads as vectors of strings:
; [graph subject predicate object type/language]

(defn nquad-string->nquad
  "Given a line from an NQuads file,
   return an NQuad vector."
  [line]
  [])

(defn nquad->nquad-string
  [[graph-iri subject-iri predicate-iri {:keys [value datatype language]}]]
  (->> [(str "<" subject-iri ">")
        (str "<" predicate-iri ">")
        (cond
         language                      (str "\"" value "\"" language)
         (= rdf:PlainLiteral datatype) (str "\"" value "\"")
         datatype                      (str "\"" value "\"^^<" datatype ">")
         :else                         (str "<" value ">"))
        (when graph-iri (str "<" graph-iri ">"))
        "."]
       (remove nil?)
       (string/join " ")))

(defn nquad->ntriple-string
  [nquad]
  )

(defn nquad->blocks
  "Given an environment and an nquad vector,
   return the updated environment and zero or more block maps."
  [{:keys [current-graph current-subject iri-labels] :as env}
   [graph subject predicate object datatype]]
  (remove
   nil?
   [; many new graph
    ; maybe new subject
    ; new statement
    ]))




;; ## JSON

; The JSON conversion is almost trivial.
; The only trick is converting certain JSON strings to Clojure keywords.

(defn block->json-string
  [block]
  )

(defn json-string->block
  [json]
  )

(defn json->parse
  "Given the JSON representation of a parse vector,
   return the EDN representation."
  [json]
  (postwalk
   (fn [form]
     (if (vector? form)
       (assoc form 0 (keyword (first form)))
       form))
   json))

(defn json->value
  [key value]
  (case key
    "block-type" (keyword value)
    "parse-tree" (json->parse value)
    "graph"      (json->parse value)
    "subject"    (json->parse value)
    "predicate"  (json->parse value)
    "datatype"   (json->parse value)
    "content"    (json->parse value)
    value))

(defn json->block
  "Given a JSON representation of block,
   return the EDN representation."
  [json]
  (->> (json/read-str json :value-fn json->value)
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))



