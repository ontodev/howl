(ns howl.core
  "Parse HOWL."
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [instaparse.core :as insta]

            [howl.util :as util]
            [howl.expression :as exp]))

;; Parsing works like this:
;;
;; 1. lines->blocks
;;     - converts a lazy seq of lines to a lazy seq of {:exp [:PARSE_TREE ...]},
;;       complete with metadata designating origin name/line-number
;;     - groups lines into blocks before parsing each block (done in one step, 'cause
;;       it's otherwise difficult to factor in the error reporting hooks we need)
;;
;; 2. (map condense-chars)
;;     - some parse blocks emit characters. As in, things like [:TAG "f" "o" "o" "b" "a" "r"].
;;       This step condenses such blocks down to [:TAG "foobar"]
;;
;; 3. (map parse-expressions)
;;     - some blocks contain sub-expressions expressed in a different syntax.
;;       The preliminary parse leaves such expressions as strings to be dealt
;;       with separately. This step deals with them.
;;
;; 4. environments
;;     - some blocks introduce new bindings to be used by other expressions. Mostly prefixes
;;       and new labels.
;;       This step extracts those bindings and puts them in the appropriate :env key.
;;     - Each environment includes the previous, and environments survive past end of file for
;;       usability reasons.
;;     - Each name is valid from the point where it's declared to the point where it's shadowed.
;;
;; 5. (map expand-block)
;;     - this step expands prefixed names into literals, and

;; Principal Datastructure
;; type block = {:env environment :exp parse-tree}
;; type parse-tree = Vector (keyword | string | parse-tree)
;; type env = {labels prefixes graph subject}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Preliminary parse
(defn group-lines
  "Given a sequence of lines, returns a lazy sequence of grouped lines"
  [lines]
  (partition-by
   (let [ct (volatile! 0)]
     #(do (when (not (or (string/blank? %) (.startsWith % "  ")))
            (vswap! ct inc))
          @ct))
   lines))

(def block-parser
  (insta/parser
   "<BLOCK> = BLANK_BLOCK / COMMENT_BLOCK /
              BASE_BLOCK / PREFIX_BLOCK /
              DEFAULT_BLOCK / LABEL_BLOCK /
              GRAPH_BLOCK / SUBJECT_BLOCK /
              ANNOTATION / LITERAL_BLOCK / LINK_BLOCK / EXPRESSION_BLOCK

    BLANK_BLOCK      = EOL
    COMMENT_BLOCK    = #'#+\\s*' #'.*' EOL
    BASE_BLOCK       = 'BASE'   SPACES BASE EOL
    PREFIX_BLOCK     = 'PREFIX' SPACES PREFIX COLON_ARROW PREFIXED EOL
    LABEL_BLOCK      = 'LABEL'  SPACES SUBJECT COLON LABEL EOL
    DEFAULT_BLOCK    = 'DEFAULT' SPACES PREDICATE SPACES
                       ('LANGUAGE' SPACES LANGUAGE | 'TYPE' SPACES DATATYPE | 'NONE')
                       EOL
    GRAPH_BLOCK      = 'GRAPH' (SPACES GRAPH)? EOL
    SUBJECT_BLOCK    = SUBJECT EOL
    ANNOTATION       = ARROWS (LITERAL_BLOCK | LINK_BLOCK)
    LITERAL_BLOCK    = PREDICATE
                       (SPACES 'LANGUAGE' SPACES LANGUAGE | SPACES 'TYPE' SPACES DATATYPE)?
                       COLON LITERAL EOL
    LINK_BLOCK       = PREDICATE COLON_ARROW OBJECT EOL
    EXPRESSION_BLOCK = PREDICATE (SPACES 'TYPE' SPACES DATATYPE)?
                       COLON_ARROWS EXPRESSION EOL

    <NAME>      = IRIREF / PREFIXED_NAME / LABEL
    <ANYNAME>   = IRIREF / BLANK_NODE_LABEL / PREFIXED_NAME / LABEL
    BASE      = IRIREF
    PREFIXED  = IRIREF
    GRAPH     = NAME
    SUBJECT   = ANYNAME
    PREDICATE = NAME
    OBJECT    = ANYNAME
    DATATYPE  = ANYNAME

    (* TERMINALS *)
    PREFIX        = #'(\\w|-)+'
    PREFIXED_NAME = PREFIX ':' #'[^\\s:/][^\\s:]*'
    COLON         = #' *' ':'  #' +'
    COLON_ARROW   = #' *' ':>' #' +'
    COLON_ARROWS  = #' *' ':>>' #' +'
    SPACES        = #' +'
    ARROWS        = #'>+' #'\\s*'
    LABEL   = !(KEYWORD | '<' | '>' | '#') (WORD SPACES?)* WORD
    KEYWORD = 'BASE' | 'GRAPH' | 'PREFIX' | 'LABEL' | 'DEFAULT'
    <WORD> = !('LANGUAGE' | 'TYPE') #'[^\\s]*[^:>\\s]'
    LITERAL       = #'(\n|.)*.+'
    EXPRESSION    = #'(?:.|\r?\n)+'
    EOL           = #'(\r|\n|\\s)*'

    (* Adapted from the NQuads spec *)
    (* WARN: Java doesn't support five-digit Unicode for \u10000-\uEFFFF ? *)
    LANGUAGE = #'[a-zA-Z]+(-[a-zA-Z0-9]+)*'
    IRIREF = '<' (#'[^\u0000-\u0020<>\"{}|^`\\\\]' | UCHAR)* '>'
    BLANK_NODE_LABEL = '_:' (PN_CHARS_U | #'[0-9]') ((PN_CHARS | '.')* PN_CHARS)?
    UCHAR = '\\\\u' HEX HEX HEX HEX | '\\\\U' HEX HEX HEX HEX HEX HEX HEX HEX
    PN_CHARS_BASE = #'[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]'
    PN_CHARS_U = PN_CHARS_BASE | '_' | ':'
    PN_CHARS = PN_CHARS_U | #'[-0-9\u00B7\u0300-\u036F\u203F-\u2040]'
    HEX = #'[0-9A-Fa-f]+'
    "))

(defn lines->blocks
  "Takes a seq of lines. Returns a seq of {:exp [:PARSE_TREE ...]} objects."
  [lines & {:keys [source] :or {source "interactive"}}]
  ((fn rec [groups ct]
     (when (not (empty? groups))
       (let [g (first groups)]
         (lazy-seq
          (cons (with-meta
                  {:exp (first (insta/parse block-parser (string/join \newline g)))}
                  {:origin {:name source :line ct}})
                (rec (rest groups) (+ ct (count g))))))))
   (group-lines lines) 1))

(defn parse-tree->string
  "Takes a parse-tree that came from the block-parser, and
   returns the corresponding string"
  [parse-tree]
  (if (string? parse-tree)
    parse-tree
    (case (first parse-tree)
      (:LABEL :PREFIX :EOL :SPACES :ABSOLUTE_IRI) (second parse-tree)
      (string/join (map parse-tree->string (rest parse-tree))))))

(defn block->string
  [block]
  (parse-tree->string (block :exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Condense step
(defn condense-tree
  "Takes a parse-tree and condenses single-character strings into a single string."
  [parse-tree]
  (if (or (string? parse-tree) (keyword? parse-tree) (nil? parse-tree) (number? parse-tree))
    parse-tree
    (case (first parse-tree)
      :IRIREF [:IRIREF
               (second parse-tree)
               (string/join (drop 2 (drop-last parse-tree)))
               (last parse-tree)]
      (:EOL) parse-tree
      (vec (map condense-tree parse-tree)))))

(defn condense-chars [block]
  (assoc block :exp (condense-tree (block :exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing expressions
(defn parse-expressions
  "Takes a parse-tree. Runs a separate parser on
   any contained :EXPRESSION_BLOCKs"
  [block]
  (if (= :EXPRESSION_BLOCK (first (block :exp)))
    (assoc
     block :exp
     (vec
      (map (fn [elem]
             (if (and (vector? elem)
                      (= :EXPRESSION (first elem)))
               (exp/string-to-parse (second elem))
               elem))
           (block :exp))))
    block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Extracting environments
(defn first-vector-starting-with
  [keyword parse-tree]
  (first (filter #(and (vector? %) (= keyword (first %))) parse-tree)))

(defn contents-of-vector
  [keyword parse-tree]
  (second (first-vector-starting-with keyword parse-tree)))

(defn name-from-node
  "Takes an environment and a node, and returns the absolute name
   contained in the node.
   Does not work on all nodes, just :BLANK_NODE, :ABSOLUTE_IRI,
   :IRIREF or :PREFIXED_NAME nodes"
  [env node]
  (case (first node)
    :BLANK_NODE (get node 2)
    (:ABSOLUTE_IRI :WORD) (second node)
    :IRIREF (let [iri (get node 2)]
              (if (.startsWith iri "http")
                iri
                (str (env :base) iri)))
    :PREFIXED_NAME (let [[_ [_ prefix] _ name] node]
                     (str (get-in env [:prefixes prefix]) name))
    :LABEL (get-in env [:labels (second node)])
    (:SUBJECT) (name-from-node env (second node))))

(defn maybe-name
  [env keyword parse-tree dest-keyword]
  (if-let [cont (contents-of-vector keyword parse-tree)]
    {dest-keyword (name-from-node env cont)}
    {}))

(defn parse-tree->names
  "Takes an environment and a parse tree.
   Returns the names found in the parse tree.
   This function needs an environment, because the given parse-tree
   might contain relative/prefixed names which need to be expanded."
  [env parse-tree]
  (case (first parse-tree)
    :PREFIX_BLOCK {:prefixes
                   {(contents-of-vector :PREFIX parse-tree)
                    (name-from-node env (contents-of-vector :PREFIXED parse-tree))}}
    :LABEL_BLOCK {:labels
                  {(contents-of-vector :LABEL parse-tree)
                   (name-from-node env (contents-of-vector :SUBJECT parse-tree))}}
    :DEFAULT_BLOCK (let [key (nth parse-tree 5)]
                     {:defaults
                      ;; TODO - the below will error when called with a label that hasn't
                      ;;        already been declared. We should do an explicit validation
                      ;;        pass at some point to catch that explicitly and provide a
                      ;;        better error message
                      {(name-from-node env (contents-of-vector :PREDICATE parse-tree))
                       (if (= key "NONE")
                         {"LANGUAGE" nil "TYPE" nil}
                         {key
                          (if (= key "LANGUAGE")
                            (contents-of-vector :LANGUAGE parse-tree)
                            (name-from-node env (contents-of-vector :DATATYPE parse-tree)))})}})
    :GRAPH_BLOCK (if-let [cont (contents-of-vector :GRAPH parse-tree)]
                   (let [g (name-from-node env cont)]
                     ;; TODO - check if we should be restoring the subject afterwards.
                     ;;        If so, this will get a bit more elaborate.
                     {:graph g :subject g})
                   {:graph nil :subject nil})
    :SUBJECT_BLOCK (maybe-name env :SUBJECT parse-tree :subject)
    :BASE_BLOCK (maybe-name env :BASE parse-tree :base)
    {}))

(defn merge-environments
  "Merges two environments.
   We could dispense with this if we wanted to introduce an extra
   level of map for the keys [:graph :subject :base]."
  [a b]
  {:prefixes (merge (a :prefixes) (b :prefixes))
   :labels (merge (a :labels) (b :labels))
   :defaults (merge-with merge (a :defaults) (b :defaults))
   :graph (if (contains? b :graph) (b :graph) (a :graph))
   :subject (or (b :subject) (a :subject))
   :base (or (b :base) (a :base))})

(defn environment-of
  "Takes an environment and a block, and returns the new environment.
   Most of the time, it'll be the same one, but some forms (such as DEFAULT,
   LABEL, PREFIX, GRAPH and SUBJECT) introduce new name associations
   which will either add to the env or shadow something from it."
  [prev-env block]
  (merge-environments prev-env (parse-tree->names prev-env (block :exp))))

(defn environments
  "Takes a sequence of blocks and a starting environment.
   Returns a sequence of blocks decorated with local environments."
  ([blocks] (environments blocks {}))
  ([blocks env]
   (when (not (empty? blocks))
     (lazy-seq
      (let [block (first blocks)
            next-env (environment-of env block)]
        (cons (assoc block :env next-env)
              (environments (rest blocks) next-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Name expansion
(defn expand-tree
  "Takes an environment and a parse tree.
   Returns a parse tree with IRIREFs and PREFIXED_NAMEs
   expanded to ABSOLUTE_IRIs"
  [env parse-tree]
  (if (or (keyword? parse-tree) (string? parse-tree) (number? parse-tree))
    parse-tree
    (case (first parse-tree)
      (:IRIREF :PREFIXED_NAME) [:ABSOLUTE_IRI (name-from-node env parse-tree)]
      (map #(expand-tree env %)))))

(defn expand-block
  [block]
  (assoc block :exp (expand-tree (block :env) (block :exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Top-level interface
(defn parse-lines
  "Takes a seq of lines, groups them appropriately, and runs the parsing step on them.
   Produces a seq of blocks ({:exp :env} maps)"
  [lines & {:keys [starting-env source]
            :or {starting-env {} source "interactive"}}]
  (->> lines
       (#(lines->blocks % :source source))
       (map condense-chars)
       ;; (map parse-expressions)
       (#(environments % starting-env))))

(defn parse-file
  "Takes a filename, and optionally a starting environment, and parses that
   file under the given environment."
  ([filename] (parse-file filename {}))
  ([filename starting-env]
   (parse-lines
    (line-seq (clojure.java.io/reader filename))
    :starting-env starting-env :source filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Error basics
(defn locate
  [block]
  (if-let [m (meta block)]
    (m :origin)
    (str block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Converting to nquads
(defn formatted
  "Takes a sting value, and returns it in an appropriate format for nquad encoding.
   - Returns angle-brace-surrounded/quoted strings as-is
   - Surrounds strings beginning with http with angle braces
   - Otherwise escapes newlines, removes two chars from subsequent lines (to account for our
     multi-line indentation convention) and quotes the input"
  [val]
  (when val
    (cond
      (.startsWith val "http") (str "<" val ">")
      (.startsWith val "<") val
      (.startsWith val "\"") val
      :else (let [split (string/split-lines val)
                  v (string/join
                     "\\n" (cons (first split)
                                 (map #(if (>= (count %) 2) (subs % 2) %)
                                      (rest split))))]
              (str "\"" v "\"")))))

(defn get-formatted
  [block keys]
  (formatted (get-in block keys)))

(defn simple-block->nquad
  "Returns an nquad from a simple block. These include any blocks
   that get translated down to one quad."
  [block]
  (let [pred (name-from-node (block :env) (contents-of-vector :PREDICATE (block :exp)))]
    [(get-formatted block [:env :subject])
     (formatted pred)
     (case (first (block :exp))
       :LITERAL_BLOCK
       (let [base (formatted (last (first-vector-starting-with :LITERAL (block :exp))))]
         (if-let [type (get-in block [:env :defaults pred "TYPE"])]
           (str base "^^" (formatted type))
           (if-let [lang (get-in block [:env :defaults pred "LANGUAGE"])]
             (str base "@" lang)
             base)))
       :LINK_BLOCK
       (formatted (name-from-node (block :env) (contents-of-vector :OBJECT (block :exp))))
       :EXPRESSION_BLOCK
       ["TODO" "expression" "block" nil]
       ;; FIXME - the default case should throw an error once we're done implementing this
       [:TODO (locate block) (first (block :exp))])
     (formatted (get-in block [:env :graph]))]))

(defn owl>
  [name]
  (str "<http://www.w3.org/2002/07/owl#" name ">"))
(defn rdf>
  [name]
  (str "<http://www.w3.org/1999/02/22-rdf-syntax-ns#" name ">"))
(defn rdf-schema>
  [name]
  (str "<http://www.w3.org/2000/01/rdf-schema#" name ">"))

(defn annotation-block->nquads
  "Returns an annotation block. Annotation blocks get encoded as multiple
   quads, and some of those depend on a target quad that is being annotated.
   The trailing quad is generated using simple-block->nquad"
  [id [source property target _] block]
  (let [name (str "_:b" id)
        base (simple-block->nquad (assoc block :exp (get (block :exp) 2)))]
    [[name (rdf> "type") (owl> "Axiom") nil]
     [name (owl> "annotatedSource") source nil]
     [name (owl> "annotatedProperty") property nil]
     [name (owl> "annotatedTarget") target nil]
     (vec (cons name (rest base)))]))

(defn nquad-relevant-blocks
  "Takes a sequence of blocks, and filters out any blocks
   that have no nquad encoding. Specifically, blocks like PREFIX or
   LABEL whose job is only establishing environment bindings, rather
   than being encoded directly in the result."
  [block-sequence]
  (filter
   #(not-any?
     #{:PREFIX_BLOCK :LABEL_BLOCK
       :BASE_BLOCK :DEFAULT_BLOCK :SUBJECT_BLOCK :GRAPH_BLOCK
       :COMMENT_BLOCK}
     (take 1 (% :exp)))
   block-sequence))

(defn find-target
  "Takes a number of leading arrows, and a target-stack, and returns
   the target quad. This gets called in the situation where several blocks
   in a row start with />+/. There might be a situation like

   foo
   > bar
   >> baz
   > mumble

   Semantically, `foo` is some quad, `bar` is an annotation on `foo`, `baz` is
   an annotation on `bar` and `mumble` is a second annotation on `foo`.
   `find-target` will return the first quad in the `target-stack` that has
   fewer leading arrows than the given `arrow-ct`."
  [arrows-ct target-stack]
  (second
   (first
    (drop-while
     #(>= (first %) arrows-ct)
     target-stack))))

(defn handle-annotation-block!
  "Takes id and stack atoms, along with a block, and handles the processing
   involved in rendering an :ANNOTATION block."
  [id stack block]
  (swap! id inc)
  (let [arrow-level (count (second (second (block :exp))))
        target (find-target arrow-level @stack)
        res (annotation-block->nquads
             @id (find-target arrow-level @stack)
             block)]
    (swap! stack #(cons [arrow-level (last res)] %))
    res))

(defn handle-simple-block!
  "Takes id and stack atoms, along with a block, and handles the processing
   involved in rendering a non-annotation block."
  [id stack block]
  (let [res (simple-block->nquad block)]
    (reset! stack (list [0 res]))
    [res]))

(defn blocks->nquads
  "Takes a sequence of blocks and emits a sequence of nquads"
  [block-sequence]
  (let [id (atom 0)
        stack (atom (list))]
    (mapcat
     #(case (first (% :exp))
        :ANNOTATION (handle-annotation-block! id stack %)
        (handle-simple-block! id stack %))
     (nquad-relevant-blocks block-sequence))))

(def quad-format "~a ~a ~a~@[~a ~].~%")

(defn nquad->string
  "Takes a single nquad and returns the appropriately formatted string."
  [[a b c d]]
  (pprint/cl-format nil quad-format a b c d))

(defn print-nquads!
  "Prints the given nquads to the given writer. If no writer is given, prints
   to standard output."
  ([nquads] (print-nquads! nquads *out*))
  ([nquads writer]
   (doseq [[a b c d] nquads]
     (pprint/cl-format writer quad-format a b c d))))

(defn nquads->file!
  "Takes a series of nquads and a filename. Writes the given nquads
   to the given file."
  [nquads filename]
  (with-open [w (clojure.java.io/writer filename)]
    (print-nquads! nquads)))
