(ns howl.core
  "Core HOWL definitions and operations."
  (:require [clojure.string :as string]
            [howl.link :as link]))

;; ## Environment

(defn reset-environment
  [env]
  (dissoc env :graph :subject))

(defn merge-environments
  [& envs]
  ; TODO: Do this properly!
  (apply merge-with merge envs))

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
  [env {:keys [label iri datatypes datatype-names] :as block}]
  (-> env
      (assoc-in [:iri-label iri] label)
      (assoc-in
       [:labels label]
       (merge
        {:iri iri}
        (when (seq datatypes) {:datatypes (vec datatypes)})))))

(defmethod update-environment :GRAPH_BLOCK
  [env {:keys [graph] :as block}]
  (assoc env :graph graph :subject graph))

(defmethod update-environment :SUBJECT_BLOCK
  [env {:keys [subject] :as block}]
  (assoc env :subject subject))

(defmethod update-environment :STATEMENT_BLOCK
  [{:keys [statement-stack] :as env}
   {:keys [arrows subject predicate object datatypes] :as block}]
  (assoc
   env
   :statement-stack
   (conj (vec (take (count arrows) statement-stack))
         [subject predicate object (first datatypes)])))

;; ## Content

(defmulti content-names->iris
  "Given an environment, a sequence of datatypes, and some content,
   use the first two datatypes to determine the method,
   then return the content converted to object form."
  (fn [env datatypes content] (vec (take 2 datatypes))))

(defmethod content-names->iris :default
  [env datatypes content]
  content)

(defmethod content-names->iris ["LINK"]
  [env datatypes content]
  (link/->iri env content))

(defmulti content-iris->names
  "Given an environment, a sequence of datatypes, and some object,
   use the first two datatypes to determine the method,
   then return the object converted to content form."
  (fn [env datatypes object] (vec (take 2 datatypes))))

(defmethod content-iris->names :default
  [env datatypes object]
  object)

(defmethod content-iris->names ["LINK"]
  [env datatypes object]
  (link/iri->name env object))

;; ## Blocks

(defmulti block-names->iris
  "Given an environment and a block,
   find IRIs for all the names,
   and return the updated block."
  (fn [env block] (:block-type block)))

(defmulti block-iris->names
  "Given an environment and a block,
   find names for all the iris,
   and return the updated block."
  (fn [env block] (:block-type block)))

(defmethod block-names->iris :default
  [env block]
  block)

(defmethod block-iris->names :default
  [env block]
  block)

(defmethod block-names->iris :LABEL_BLOCK
  [env {:keys [datatype-names target-name] :as block}]
  (assoc
   block
   :datatypes (map (partial link/unpack-datatype env) datatype-names)
   :iri (link/->iri env target-name)))

(defmethod block-iris->names :LABEL_BLOCK
  [env {:keys [datatypes iri] :as block}]
  (assoc
   block
   :datatype-names (map (partial link/iri->name env) datatypes)
   :target-name (link/iri->name env iri)))

(defmethod block-names->iris :GRAPH_BLOCK
  [env {:keys [graph-name] :as block}]
  (let [iri (when graph-name (link/->iri env graph-name))]
    (assoc block :graph iri :subject iri)))

(defmethod block-iris->names :GRAPH_BLOCK
  [env {:keys [graph] :as block}]
  (assoc block :graph-name (when graph (link/iri->name env graph))))

(defmethod block-names->iris :SUBJECT_BLOCK
  [env {:keys [subject-name] :as block}]
  (assoc block :subject (link/->iri env subject-name)))

(defmethod block-iris->names :SUBJECT_BLOCK
  [env {:keys [subject] :as block}]
  (assoc block :subject-name (link/iri->name env subject)))

(defmethod block-names->iris :STATEMENT_BLOCK
  [{:keys [graph subject] :as env}
   {:keys [predicate-name datatype-names content] :as block}]
  (let [datatypes
        (->> datatype-names (map (partial link/unpack-datatype env)) vec)]
    (assoc
     block
     :graph graph
     :subject subject ; TODO: handle missing subject
     :predicate (link/->iri env predicate-name)
     :datatypes datatypes
     :object (when content (content-names->iris env datatypes content)))))

(defmethod block-iris->names :STATEMENT_BLOCK
  [env {:keys [predicate datatypes object] :as block}]
  (assoc
   block
   :predicate-name (link/iri->name env predicate)
   :datatype-names (->> datatypes (map (partial link/iri->name env)) vec)
   :content (when object (content-iris->names env datatypes object))))
