(ns howl.link
  "Tools for working with IRIs, prefixed names, labels, and datatypes."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [cemerick.url :refer [url]]))

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
KEYWORD = 'BASE' | 'GRAPH' | 'PREFIXES' | 'LABELS' | 'DEFAULT' | 'LINK'
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
   :LABEL  (fn [& xs] [:LABEL (->> xs flatten (filter string?) (apply str))])})

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
  (when-not (get-in env [:labels label :iri])
    (throw (Exception. (format "Could not find label '%s'." label))))
  ; Should be absolute when assigned.
  (get-in env [:labels label :iri]))

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


;; ## Datatypes

(def rdf:PlainLiteral "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")

(def default-datatype-map
  {:datatype-iri rdf:PlainLiteral
   :language nil})

(defn unpack-datatype
  "Given an environment and a datatype parse,
   return a map with :datatype-iri and :language keys,
   or nil."
  [env datatype]
  (cond
   (= "LINK" datatype)
   {:datatype-iri nil
    :language nil}

   (= :LANGUAGE (first datatype))
   {:datatype-iri rdf:PlainLiteral
    :language (last datatype)}

   datatype
   {:datatype-iri (name->iri env datatype)
    :language nil}

   :else
   default-datatype-map))


