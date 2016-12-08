(ns howl.link
  "Tools for working with IRIs, prefixed names, labels, and datatypes."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [howl.util :as util]))

;; ## Links
;
; These are tools for working with links,
; which occur in many of the supported HOWL formats.

; TODO: Fix label grammar
; TODO: Fix prefixed name

(def link-grammar "
LINK            = NAME_OR_BLANK
<NAME_OR_BLANK> = IRIREF / BLANK_NODE_LABEL / PREFIXED_NAME / LABEL
<NAME>          = IRIREF / PREFIXED_NAME / LABEL
<IRI>           = IRIREF / PREFIXED_NAME

(* Adapted from the NQuads spec *)
(* WARN: Java doesn't support five-digit Unicode for \u10000-\uEFFFF *)
IRIREF           = '<' (#'[^\u0000-\u0020<>\"{}|^`\\\\]' | UCHAR)* '>'
BLANK_NODE_LABEL = '_:' (PN_CHARS_U | #'[0-9]') ((PN_CHARS | '.')* PN_CHARS)?
UCHAR            = '\\\\u' HEX HEX HEX HEX | '\\\\U' HEX HEX HEX HEX HEX HEX HEX HEX
PN_CHARS_BASE    = #'[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]'
PN_CHARS_U       = PN_CHARS_BASE | '_' | ':'
PN_CHARS         = PN_CHARS_U | #'[-0-9\u00B7\u0300-\u036F\u203F-\u2040]'
HEX              = #'[0-9A-Fa-f]+'

PREFIXED_NAME = #'(\\w|-)+' ':' #'[^\\s:/][^\\s:\\]]*'
PREFIX        = #'(\\w|-)+'

LABEL   = !(KEYWORD | '<' | '>' | '[' | ']' | '#') (WORD SPACES?)* WORD
KEYWORD = 'BASE' | 'GRAPH' | 'PREFIX' | 'LABEL' | 'DEFAULT' | 'LINK' | 'PLAIN'
<WORD>  = #'[^|\\s]*[^|:\\]\\s]'
SPACES  = #' +'

LINK_OR_DATATYPES = LINK | DATATYPES
DATATYPES = '' | #' +\\[ *' DATATYPE (#' +/ +' DATATYPE)* #' *]'
<DATATYPE> = ('PLAIN' | 'LINK' | LANGUAGE_TAG | NAME)
LANGUAGE_TAG    = '@' LANGUAGE_CODE
<LANGUAGE_CODE> = #'[a-zA-Z]+(-[a-zA-Z0-9]+)*'")

(def link-transformations
  {:IRIREF
   (fn [& xs] [:IRIREF "<" (apply str (rest (butlast xs))) ">"])
   :BLANK_NODE_LABEL
   (fn [& xs] [:BLANK_NODE_LABEL "_:" (->> (rest xs) flatten (filter string?) (apply str))])
   :LABEL
   (fn [& xs] [:LABEL (->> xs flatten (filter string?) (apply str))])})

(def link-parser (insta/parser link-grammar))

(defn parse-link
  [content & options]
  (let [result (apply link-parser content options)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Link parser failure")))
    (->> result
         (insta/transform link-transformations)
         second)))

(defn datatype-names->parse
  "Given some datatype-names, return a parse vector."
  [datatype-names]
  (cond
    (nil? (seq datatype-names))
    [:DATATYPES]
    :else
    (concat
     [:DATATYPES " [" (first datatype-names)]
     (mapcat (fn [dt] [" / " dt]) (rest datatype-names))
     ["]"])))

(defn parse->datatype-names
  "Given a datatype parse vector, return just the datatype-names."
  [parse]
  (->> parse
       rest
       (remove #(and (string? %) (re-matches #"( |\[|\]|/)*" %)))
       vec))

(defn blank?
  [iri]
  (and (string? iri)
       (util/starts-with? iri "_:")))

(defn random-blank-node
  []
  (str
   "_:"
   #?(:clj (java.util.UUID/randomUUID)
      :cljs (random-uuid))))

; ## Blank Nodes
;
; HOWL uses random blank nodes,
; but sometimes we want them to be predictable.
; This function maps blank nodes IRIs to new sequential nodes.

(defn replace-blank
  "Given a map from blank-node-before to blank-node-after
   with a :counter atom,
   and an IRI and datatype,
   if the IRI is a blank node then try to map it,
   creating a mapping to a new sequential blank node if required.
   Return an updated mapping and the replaced IRI."
  [{:keys [counter] :as coll} iri datatype]
  (if (and (= "LINK" datatype) (blank? iri))
    (if (find coll iri)
      [coll (get coll iri)]
      (let [b (str "_:b" (swap! counter inc))]
        [(assoc coll iri b) b]))
    [coll iri]))

(defn resolve-iri
  [{:keys [base] :or {base "file:///howl-unspecified-base/"}} iri]
  (if (util/absolute-uri-string? iri)
    iri
    (str base iri)))

(defmulti ->iri
  "Given an environment and a parse tree,
   returned the resolved, absolute IRI contained in it.
   Only works on IRIREFs, PREFIXED_NAMEs and LABELs"
  (fn [env parse-tree] (first parse-tree)))

(defmethod ->iri :default
  [env parse-tree]
  (util/throw-exception "problem with ->iri:" parse-tree))

(defmethod ->iri :IRIREF
  [env [_ _ iri _]]
  (resolve-iri env iri))

(defmethod ->iri :PREFIXED_NAME
  [env [_ prefix _ local]]
  (when-not (get-in env [:prefix-iri prefix])
    (util/throw-exception (format "Could not find prefix '%s'" prefix)))
  (resolve-iri env (str (get-in env [:prefix-iri prefix]) local)))

(defmethod ->iri :LABEL
  [env [_ label]]
  (when-not (get-in env [:labels label :iri])
    (util/throw-exception (format "Could not find label '%s'." label)))
  ; Should be absolute when assigned.
  (get-in env [:labels label :iri]))

(defn name->label
  "Given a name parse, return a label string
   or nil if the name is not a label."
  [parse]
  (when (= :LABEL (first parse)) (second parse)))

(defn find-prefix
  "Given an environment with a :iri-prefix map,
   return the [prefix-iri prefix] pair
   for which the given iri starts with the prefix-iri,
   and prefix-iri is the longest available."
  [env iri]
  (->> env
       :iri-prefix
       (sort-by (comp count first) >)
       (filter
        (fn [[prefix-iri prefix]]
          (util/starts-with? iri prefix-iri)))
       first))

(defn iri->name
  "Given an environment and an IRI,
   return the best available name parse:
   LINK, blank node, language, label, prefixed name, relative IRI,
   or the (absolute) that was given IRI."
  [{:keys [base] :as env} iri]
  (cond
    (= "PLAIN" iri) "PLAIN"

    (= "LINK" iri) "LINK"

    (blank? iri) [:BLANK_NODE_LABEL "_:" (subs iri 2)]

    (util/starts-with? iri "@") [:LANGUAGE_TAG "@" (subs iri 1)]

    (get-in env [:iri-label iri]) [:LABEL (get-in env [:iri-label iri])]

    (find-prefix env iri)
    (let [[prefix-iri prefix] (find-prefix env iri)]
      [:PREFIXED_NAME prefix ":" (subs iri (count prefix-iri))])

    (and base (string? base) (util/starts-with? iri base))
    [:IRIREF "<" (subs iri (count base)) ">"]

    :else [:IRIREF "<" iri ">"]))

(defn unpack-datatype
  [env datatype]
  (cond
    (nil? datatype) nil
    (= "PLAIN" datatype) "PLAIN"
    (= "LINK" datatype) "LINK"
    (= :LANGUAGE_TAG (first datatype)) (str "@" (last datatype))
    :else (->iri env datatype)))

(defn unpack-format
  [env format]
  (cond
    (nil? format) nil
    (= "PLAIN" format) "PLAIN"
    (= "LINK" format) "LINK"
    :else (->iri env format)))
