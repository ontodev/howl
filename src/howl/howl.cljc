(ns howl.howl
  "Convert HOWL to and from HOWL syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk keywordize-keys]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]

            [howl.link :as link]
            [howl.core :as core]
            [howl.util :as util]))

(def block-grammar
  (str "
<BLOCK> = WHITESPACE
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

(defmulti block->parse
  "Given a block, return a new parse-tree."
  :block-type)

(defmulti parse->block
  "Given a parse-tree,
   return a block map for this block type."
  first)

(defmulti normalize-whitespace
  :block-type)

(defmethod normalize-whitespace :default
  [block]
  (assoc
   block
   :leading-whitespace ""
   :trailing-whitespace "\n"))

(defmethod block->parse :COMMENT_BLOCK
  [{:keys [comment] :as block}]
  [:COMMENT_BLOCK comment])

(defmethod parse->block :COMMENT_BLOCK
  [[_ comment]]
  {:comment comment})

(defmethod block->parse :PREFIX_BLOCK
  [{:keys [prefix iri] :as block}]
  [:PREFIX_BLOCK
   "PREFIX"
   [:SPACES " "]
   [:PREFIX prefix]
   [:COLON "" ":" " "]
   [:IRIREF "<" iri ">"]])

(defmethod parse->block :PREFIX_BLOCK
  [[_ _ _ [_ prefix] _ [_ _ iri _]]]
  (link/check-iri iri)
  {:prefix prefix :iri iri})

(defn datatype-format->parse
  [datatype-name format-name]
  (cond
    (and datatype-name format-name)
    [:DATATYPE " [" datatype-name "|" format-name "]"]
    datatype-name
    [:DATATYPE " [" datatype-name "]"]
    :else
    [:DATATYPE]))

(defmethod block->parse :LABEL_BLOCK
  [{:keys [label datatype-name format-name target-name] :as block}]
  [:LABEL_BLOCK
   "LABEL"
   [:SPACES " "]
   [:LABEL "type"]
   (datatype-format->parse datatype-name format-name)
   [:COLON "" ":" " "]
   target-name])

(defmethod parse->block :LABEL_BLOCK
  [[_ _ _ [_ label] [_ _ datatype-name _ format-name _] _ target]]
  {:label label
   :datatype-name datatype-name
   :format-name format-name
   :target-name target})

(defmethod block->parse :BASE_BLOCK
  [{:keys [base] :as block}]
  [:BASE_BLOCK
   "BASE"
   [:SPACES " "]
   [:IRIREF "<" base ">"]])

(defmethod parse->block :BASE_BLOCK
  [[_ _ _ [_ _ iri _]]]
  {:base iri})

(defmethod block->parse :GRAPH_BLOCK
  [{:keys [graph-name] :as block}]
  (if graph-name
    [:GRAPH_BLOCK "GRAPH" [:SPACES " "] graph-name]
    [:GRAPH_BLOCK "DEFAULT GRAPH"]))

(defmethod parse->block :GRAPH_BLOCK
  [[_ _ _ name]]
  {:graph-name name})

(defmethod normalize-whitespace :GRAPH_BLOCK
  [block]
  (assoc
   block
   :leading-whitespace "\n"
   :trailing-whitespace "\n"))

(defmethod block->parse :SUBJECT_BLOCK
  [{:keys [subject-name] :as block}]
  [:SUBJECT_BLOCK subject-name])

(defmethod parse->block :SUBJECT_BLOCK
  [[_ name]]
  {:subject-name name})

(defmethod normalize-whitespace :SUBJECT_BLOCK
  [block]
  (assoc
   block
   :leading-whitespace "\n"
   :trailing-whitespace "\n"))

(defn indent
  [content]
  (let [lines (string/split-lines content)]
    (->> (rest lines)
         (map #(if (string/blank? %) % (str "  " %)))
         (concat [(first lines)])
         (string/join \newline))))

(defmethod block->parse :STATEMENT_BLOCK
  [{:keys [arrows predicate-name datatype-name format-name content] :as block}]
  [:STATEMENT_BLOCK
   (if (string/blank? arrows)
     [:ARROWS "" ""]
     [:ARROWS arrows " "])
   predicate-name
   (datatype-format->parse datatype-name format-name)
   [:COLON "" ":" " "]
   (if (= "LINK" datatype-name) content (indent content))])

(defmethod parse->block :STATEMENT_BLOCK
  [[_ [_ arrows _] predicate-name [_ _ datatype-name _ format-name _] _ content]]
  {:arrows arrows
   :predicate-name predicate-name
   :datatype-name datatype-name
   :format-name format-name
   :unparsed-content content})

;; ## Content

(defmulti parse-content
  "Given an environment,
   a format IRI string (nil when the object is a literal)
   and an unparsed content string,
   return the content as a parse tree."
  (fn [env format unparsed] format))

; The default behaviour is to remove indentation.

(defmethod parse-content :default
  [env format unparsed]
  (string/replace unparsed #"(?m)^  |^ " ""))

(defmethod parse-content "LINK"
  [env format unparsed]
  (link/parse-link unparsed))

(defn update-content
  "Given an environment and a block with resolved :format IRI,
   replace :unparsed-content with parsed :content and :object,
   and updated :parse-tree."
  [env {:keys [parse-tree format unparsed-content] :as block}]
  (if unparsed-content
    (let [content (parse-content env format unparsed-content)]
      (assoc
       (dissoc block :unparsed-content)
       :parse-tree (assoc parse-tree 5 content) ; splice content into parse-tree
       :content content
       :object (core/content-names->iris env format content)))
    block))

(defn update-annotation
  "Given an environment with an :statement-stack and a block with :arrows,
   return the block with an :annotation-target value (maybe nil)."
  [{:keys [statement-stack] :as env} {:keys [arrows] :as block}]
  (if (string/blank? arrows)
    block
    (assoc
     block
     :subject (link/new-blank-node)
     :annotation-target
     (get statement-stack (dec (count arrows))))))

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
   return the updated block map."
  [{:keys [source line] :as env} source-string]
  (let [[[_ lws] parse-tree [_ tws]] (parse-block source-string)]
    (->> parse-tree
         parse->block
         (merge
          {:block-type (first parse-tree)
           :source source
           :line line
           :string source-string
           :parse-tree parse-tree
           :leading-whitespace lws
           :trailing-whitespace tws})
         (core/block-names->iris env)
         (update-content env)
         (update-annotation env))))

(defn lines->blocks
  "Given a sequence of lines,
   group them, process them,
   and return a lazy sequence of block maps."
  [{:keys [source] :or {source "interactive"} :as env} lines]
  (reduce
   (fn [env lines]
     (let [block (process-block env (apply str lines))]
       (-> (core/update-environment env block)
           (update-in [:blocks] (fnil conj []) block)
           (update-in [:line] + (count lines)))))
   (assoc env :source source :line 1)
   (group-lines lines)))

(defn update-parse-tree
  [block]
  (assoc block :parse-tree (block->parse block)))

(defn update-whitespace
  [blocks]
  (let [blocks (map normalize-whitespace blocks)]
    (assoc-in (vec blocks) [0 :leading-whitespace] "")))

(defn block->howl-string
  [{:keys [leading-whitespace parse-tree trailing-whitespace]}]
  (str leading-whitespace
       (->> parse-tree flatten (filter string?) (apply str))
       trailing-whitespace))
