(ns howl.core
  "Core HOWL definitions and operations."
  (:require [clojure.string :as string]
            [howl.link :as link]))


;; ## Environment

(defn reset-environment
  [env]
  (dissoc env :current-graph-iri :current-subject-iri))

(defmulti update-environment
  "Given an environment and a block,
   use the block to update the environment,
   then return the updated environment."
  (fn [env block] (:block-type block)))

(defmethod update-environment :default
  [env block]
  env)

(defmethod update-environment :PREFIX_BLOCK
  [env {:keys [prefix iri] :as block}]
  (-> env
      (assoc-in [:prefix-iri prefix] iri)
      (assoc-in [:iri-prefix iri] prefix)))

(defmethod update-environment :BASE_BLOCK
  [env {:keys [base] :as block}]
  (assoc env :base base))

(defmethod update-environment :LABEL_BLOCK
  [env {:keys [label iri datatype] :as block}]
  (-> (if datatype
        (assoc-in env [:labels label :datatype] datatype)
        env)
      (assoc-in [:labels label :iri] iri)
      (assoc-in [:iri-label iri] label)))

(defmethod update-environment :GRAPH_BLOCK
  [env {:keys [graph] :as block}]
  (assoc env :graph graph))

(defmethod update-environment :SUBJECT_BLOCK
  [env {:keys [subject] :as block}]
  (assoc env :subject subject))


;; ## Content

(defmulti content->parse
  "Given an environment,
   a datatype IRI string (or nil when the object is an IRI),
   and a content string,
   return the object and the parse tree for the content."
  (fn [env datatype content] datatype))

; The default behaviour is to remove indentation.

(defmethod content->parse :default
  [env datatype content]
  (let [unindented (string/replace content #"(?m)^  |^ " "")]
    [unindented unindented]))

(defmethod content->parse "LINK"
  [env datatype content]
  (let [result (link/parse-link content)]
    [(link/name->iri env result) result]))


;; ## Blocks

(defmulti names->iris
  "Given an environment and a block,
   find IRIs for all the names,
   and return the updated block."
  (fn [env block] (:block-type block)))

(defmethod names->iris :default
  [env block]
  block)

(defmethod names->iris :LABEL_BLOCK
  [env {:keys [datatype-name target-name] :as block}]
  (assoc
   block
   :datatype (link/unpack-datatype env datatype-name)
   :iri (link/id->iri env target-name)))

(defmethod names->iris :GRAPH_BLOCK
  [env {:keys [graph-name] :as block}]
  (assoc block :graph (when graph-name (link/name->iri env graph-name))))

(defmethod names->iris :SUBJECT_BLOCK
  [env {:keys [subject-name] :as block}]
  (assoc block :subject (link/name->iri env subject-name)))

(defmethod names->iris :STATEMENT_BLOCK
  [{:keys [graph subject] :as env}
   {:keys [parse-tree predicate-name datatype-name content] :as block}]
  (let [predicate-label (link/name->label predicate-name)
        datatype
        (or (when datatype-name (link/unpack-datatype env datatype-name))
            (when predicate-label
              (get-in env [:labels predicate-label :datatype])))
        [object content] (content->parse env datatype content)]
    (assoc
     block
     :parse-tree (assoc parse-tree 5 content) ; splice content into parse-tree
     :content content
     :graph graph
     :subject subject ; TODO: handle missing subject
     :predicate (link/name->iri env predicate-name)
     :object object
     :datatype datatype)))
