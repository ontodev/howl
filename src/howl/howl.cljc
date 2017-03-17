(ns howl.howl
  "Convert HOWL to and from HOWL syntax."
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk keywordize-keys]]
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
LABEL_BLOCK     = 'LABEL' SPACES LABEL DATATYPES COLON IRI
BASE_BLOCK      = 'BASE' SPACES IRIREF
GRAPH_BLOCK     = 'DEFAULT GRAPH' | 'GRAPH' (SPACES NAME)?
SUBJECT_BLOCK   = NAME_OR_BLANK
STATEMENT_BLOCK = ARROWS NAME DATATYPES COLON STATEMENT_LINE

<STATEMENT_LINE> = #'(\n*.+)+'

WHITESPACE  = #'(\\r|\\n|\\s)*'
COLON       = #' *' ':'  #'(\n| )+'
ARROWS      = #'>*' #'\\s*'"
       link/link-grammar))

;; (def grouped
;;   (howl/group-lines
;;    (concat
;;     (line-seq (io/reader "test/typed-context/context.howl"))
;;     (line-seq (io/reader "test/typed-context/test1.howl")))))

;; (map #(howl/block-parser (string/join \newline %)) grouped)
;; (map howl/line-group->basic-parse grouped)

;; (map
;;  #(if (= %1 %2)
;;     (println "k")
;;     (do (println (str %1))
;;         (println " ==>" (str %2))))
;;  (map #(howl/block-parser (string/join \newline %)) grouped)
;;  (map howl/line-group->basic-parse grouped))

(defn string->iriref [line]
  (let [iriref (re-find #"^<[^\u0000-\u0020<>\"{}|^`\\\\]*>" line)]
    (when iriref (vec (cons :IRIREF (map str iriref))))))

(defn string->prefixed-name [line]
  (let [[_ prefix name] (re-find #"^(\w+):([^\s:/][^\s:\\]*)$" line)]
    (when (and prefix name)
      [:PREFIXED_NAME prefix ":" name])))

(defn string->label [line]
  (when (re-find #"^[^<>\[\]#|\s][^<>\[\]#|\s:]+$" line)
    [:LABEL line]))

(defn string->name [line]
  (or (string->iriref line)
      (string->prefixed-name line)
      (string->label line)))

(defn string->name-or-blank [line]
  (if-let [blank (re-find #"^_:[-0-9\u00B7\u0300-\u036F\u203F-\u2040A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]+$" line)]
    [:BLANK blank]
    (string->name line)))

(defn string->datatype [line]
  (when line
    (if-let [[_ a word] (re-find #"(\s*\[)(PLAIN|LINK)" line)]
      (list a word "]")
      (if-let [[_ a lang] (re-find #"(\s*\[)@(.*)\]" line)]
        (list a [:LANGUAGE_TAG "@" lang] "]")
        (let [[_ a name] (re-find #"(\s*\[)(.*?)\]" line)]
          (list a (string->name name) "]"))))))

(defn sdrop [ct string]
  (if (>= (count string) ct)
    (subs string 2)
    string))

(defn line-group->statement [line-group]
  (if-let [[_ a arrows name-string datatypes b statement] (re-find #"(\s*)(>*)\s*(.*?)(\s*\[.*?\])?:(\s+)(.*)" (first line-group))]
    (if-let [name (string->name name-string)]
      (list
       [:WHITESPACE a]
       [:STATEMENT_BLOCK
        [:ARROWS a arrows] name
        (vec (cons :DATATYPES (string->datatype datatypes)))
        [:COLON "" ":" b]
        (string/join \newline (cons statement (rest line-group)))]
       [:WHITESPACE ""]))))

(defn line-group->basic-parse
  "Takes a line group and returns a { :origin-string, :block-type, :parse-tree }
  Where the origin string is the input joined by \\newline the :block-type is a symbol designating the specific block and the :parse tree is a basic, un-processed vector tree representing a Howl block."
  [line-group]
  (let [first-line (first line-group)
        first-word (re-find #"\w+" first-line)
        basic-parse
        (cond (util/starts-with? first-line "#")
              (list
               [:WHITESPACE ""]
               [:COMMENT_BLOCK
                (string/join
                 \newline
                 (cons first-line
                       (map #(sdrop 2 %)
                            (rest line-group))))]
               [:WHITESPACE ""])

              (= first-word "PREFIX")
              (let [[_ a b name c iriref] (re-find #"(\s*)PREFIX(\W+)(\w+):( +)(.*)" first-line)]
                (list
                 [:WHITESPACE a]
                 [:PREFIX_BLOCK
                  "PREFIX"
                  [:SPACES b]
                  [:PREFIX name]
                  [:COLON "" ":" c]
                  (string->iriref iriref)]
                 [:WHITESPACE ""]))

              (= first-word "LABEL")
              (let [[_ a b name datatypes c iri-or-prefixed]
                    (re-find #"(\s*)LABEL( +)(\w+)(\W*\[.*\])?:(\W+)(.*)" first-line)]
                (list
                 [:WHITESPACE a]
                 [:LABEL_BLOCK
                  "LABEL"
                  [:SPACES b]
                  [:LABEL name]
                  (vec (cons :DATATYPES (string->datatype datatypes)))
                  [:COLON "" ":" c]
                  (or (string->iriref iri-or-prefixed)
                      (string->prefixed-name iri-or-prefixed))]
                 [:WHITESPACE ""]))

              (= first-word "BASE")
              (let [[_ a b iriref] (re-find #"(\s*)BASE( +)(.*)" first-line)]
                (list
                 [:WHITESPACE a]
                 [:BASE_BLOCK
                  "BASE"
                  [:SPACES b]
                  (string->iriref iriref)]
                 [:WHITESPACE "\n"]))

              (= first-word "GRAPH")
              (let [[_ a b name] (re-find #"(\s*)GRAPH( +)(.*)" first-line)]
                (list
                 [:WHITESPACE a]
                 [:GRAPH_BLOCK
                  "GRAPH"
                  [:SPACES b]
                  (string->name name)]
                 [:WHITESPACE "\n"]))

              :else (if-let [name-parse
                             (and (empty? (rest line-group))
                                  (string->name-or-blank first-line))]
                      (let [[_ leading-whitespace] (re-find #"^(\s*)" first-line)]
                        (list [:WHITESPACE leading-whitespace]
                              [:SUBJECT_BLOCK
                               name-parse]
                              [:WHITESPACE ""]))
                      (or (line-group->statement line-group)
                          [:UNPARSED_BLOCK (string/join \newline line-group)])))]
    basic-parse))

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
  {:prefix prefix :iri iri})

(defmethod block->parse :LABEL_BLOCK
  [{:keys [label datatype-names target-name] :as block}]
  [:LABEL_BLOCK
   "LABEL"
   [:SPACES " "]
   [:LABEL "type"]
   (link/datatype-names->parse datatype-names)
   [:COLON "" ":" " "]
   target-name])

(defmethod parse->block :LABEL_BLOCK
  [[_ _ _ [_ label] datatype-parse _ target]]
  {:label label
   :datatype-names (link/parse->datatype-names datatype-parse)
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
  (let [lines (string/split-lines (or content ""))]
    (->> (rest lines)
         (map #(if (string/blank? %) % (str "  " %)))
         (concat [(first lines)])
         (string/join \newline))))

(defmethod block->parse :STATEMENT_BLOCK
  [{:keys [arrows predicate-name use-default-datatypes datatype-names content]
    :as block}]
  [:STATEMENT_BLOCK
   (if (string/blank? arrows)
     [:ARROWS "" ""]
     [:ARROWS arrows " "])
   predicate-name
   (if use-default-datatypes
     [:DATATYPES]
     (link/datatype-names->parse datatype-names))
   [:COLON "" ":" " "]
   (if (= "LINK" (first datatype-names)) content (indent content))])

(defmethod parse->block :STATEMENT_BLOCK
  [[_ [_ arrows _] predicate-name datatype-parse _ content]]
  {:arrows arrows
   :predicate-name predicate-name
   :use-default-datatypes (= datatype-parse [:DATATYPES])
   :datatype-names (link/parse->datatype-names datatype-parse)
   :unparsed-content content})

;; ## Content

(defmulti parse-content
  "Given an environment,
   a sequence of datatypes (or empty when the object is a literal),
   and an unparsed content string,
   return the content as a parse tree."
  (fn [env datatypes unparsed] (vec (take 2 datatypes))))

; The default behaviour is to remove indentation.

(defmethod parse-content :default
  [env datatypes unparsed]
  (string/replace unparsed #"(?m)^  |^ " ""))

(defmethod parse-content ["LINK"]
  [env datatypes unparsed]
  (link/parse-link unparsed))

(defn update-content
  "Given an environment and a block with resolved :datatypes,
   decide whether to use the default datatypes or not,
   then update the block."
  [env
   {:keys [parse-tree predicate-name unparsed-content
           use-default-datatypes datatypes]
    :as block}]
  (if unparsed-content
    (let [predicate-label (link/name->label predicate-name)
          predicate-datatypes
          (get-in env [:labels predicate-label :datatypes] ["PLAIN"])
          datatypes (if use-default-datatypes predicate-datatypes datatypes)
          content (parse-content env datatypes unparsed-content)]
      (assoc
       (dissoc block :unparsed-content)
       :datatypes datatypes
       :datatype-names (->> datatypes (map (partial link/iri->name env)) vec)
       :parse-tree (assoc parse-tree 5 content) ; splice content into parse-tree
       :content content
       :object (core/content-names->iris env datatypes content)))
    block))

(defn update-annotation
  "Given an environment with an :statement-stack and a block with :arrows,
   return the block with an :annotation-target value (maybe nil)."
  [{:keys [statement-stack] :as env} {:keys [arrows] :as block}]
  (if (string/blank? arrows)
    block
    (assoc
     block
     :subject (link/random-blank-node)
     :annotation-target
     (get statement-stack (dec (count arrows))))))

(defn group-lines
  "Given a sequence of lines, returns a lazy sequence of grouped lines"
  [lines]
  (partition-by
   (let [ct (volatile! 0)]
     #(do (when (not (or (string/blank? %) (util/starts-with? % "  ")))
            (vswap! ct inc))
          @ct))
   lines))

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
  "Given an environment and a sequence of lines,
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

(defn process-lines!
  "Given an environment, a sequence of lines (strings),
   and a block processing function (probably with side-effects),
   group the lines, convert them to block maps,
   call the processing function on each.
   After processing all lines, return updated environment."
  [{:keys [source] :or {source "interactive"} :as env}
   lines
   process-block!]
  (reduce
   (fn [env lines]
     (let [block (process-block env (apply str lines))]
       (process-block! block)
       (-> (core/update-environment env block)
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
