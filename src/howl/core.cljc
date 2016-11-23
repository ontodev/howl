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

(defmethod update-environment :STATEMENT_BLOCK
  [{:keys [statement-stack] :as env}
   {:keys [arrows subject predicate object datatype] :as block}]
  (assoc
   env
   :statement-stack
   (conj (vec (take (count arrows) statement-stack))
         [subject predicate object datatype])))

;; ## Blocks

(defmulti names->iris
  "Given an environment and a block,
   find IRIs for all the names,
   and return the updated block."
  (fn [env block] (:block-type block)))

(defmulti iris->names
  "Given an environment and a block,
   find names for all the iris,
   and return the updated block."
  (fn [env block] (:block-type block)))

(defmethod names->iris :default
  [env block]
  block)

(defmethod iris->names :default
  [env block]
  block)

(defmethod names->iris :LABEL_BLOCK
  [env {:keys [datatype-name target-name] :as block}]
  (assoc
   block
   :datatype (link/unpack-datatype env datatype-name)
   :iri (link/id->iri env target-name)))

(defmethod iris->names :LABEL_BLOCK
  [env {:keys [datatype iri] :as block}]
  (assoc
   block
   :datatype-name (link/iri->name env datatype)
   :target-name (link/iri->name env iri)))

(defmethod names->iris :GRAPH_BLOCK
  [env {:keys [graph-name] :as block}]
  (assoc block :graph (when graph-name (link/name->iri env graph-name))))

(defmethod iris->names :GRAPH_BLOCK
  [env {:keys [graph] :as block}]
  (assoc block :graph-name (when graph (link/iri->name env graph))))

(defmethod names->iris :SUBJECT_BLOCK
  [env {:keys [subject-name] :as block}]
  (assoc block :subject (link/name->iri env subject-name)))

(defmethod iris->names :SUBJECT_BLOCK
  [env {:keys [subject] :as block}]
  (assoc block :subject-name (link/iri->name env subject)))

(defmethod names->iris :STATEMENT_BLOCK
  [{:keys [graph subject] :as env}
   {:keys [parse-tree predicate-name datatype-name] :as block}]
  (let [predicate-label (link/name->label predicate-name)]
    (assoc
     block
     :graph graph
     :subject subject ; TODO: handle missing subject
     :predicate (link/name->iri env predicate-name)
     :datatype
     (or (when datatype-name (link/unpack-datatype env datatype-name))
         (when predicate-label
           (get-in env [:labels predicate-label :datatype]))))))

(defmethod iris->names :STATEMENT_BLOCK
  [env {:keys [predicate datatype] :as block}]
  (assoc
   block
   :predicate-name (link/iri->name env predicate)
   :datatype-name ; compare to predicate's default datatype
   (let [label   (get-in env [:iri-label predicate])
         default (get-in env [:labels label :datatype])]
     (when-not (= datatype default)
       (link/iri->name env datatype)))))
