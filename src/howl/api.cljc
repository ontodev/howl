(ns howl.api
  "Cross-platform API for HOWL."
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.howl :as howl]
            [howl.nquads :as nquads]
            [howl.rdf-list :as rdf-list]
            [howl.manchester :as manchester]
            [howl.util :as util]))

(defn append-newline
  [s]
  (str s \newline))

(defn parse-howl-strings
  "Given one or more environment maps or HOWL strings,
   return an environment with a :blocks sequence."
  [& args]
  (reduce
   (fn [env arg]
     (cond
       (or (map? arg) (nil? arg))
       (core/merge-environments env arg)

       (or (seq? arg) (string? arg))
       (->> (if (seq? arg) arg (string/split-lines arg))
            (map append-newline)
            (howl/lines->blocks (core/reset-environment env)))

       :else (util/throw-exception "Unexpected input -- " arg)))
   {}
   args))

(defn parse-nquads-strings
  "Given one or more environment maps or NQuads strings,
   return an environment with a :blocks sequence."
  [& args]
  (reduce
   (fn [env arg]
     (cond
       (or (map? arg) (nil? arg))
       (core/merge-environments env arg)

       (or (seq? arg) (string? arg))
       (->> (if (seq? arg) arg (string/split-lines arg))
            (nquads/lines->blocks (core/reset-environment env)))

       :else (util/throw-exception "Unexpected input -- " arg)))
   {}
   args))

(defn ^:export howl-to-environment
  "Given one or more environment maps or
   input strings in HOWL format,
   return an environment map."
  [& args]
  (dissoc
   (apply parse-howl-strings args)
   :blocks))

(defn howl-to-nquad-vectors
  [& args]
  (let [env (apply parse-howl-strings args)
        nquads (mapcat nquads/block->nquads (:blocks env))]
    (->> (if (get-in env [:options :sequential-blank-nodes])
           (nquads/sequential-blank-nodes nquads)
           nquads))))

(defn ^:export howl-to-nquads
  "Given one or more environments or HOWL strings,
   return a string of N-Quads."
  [& args]
  (->> (apply howl-to-nquad-vectors args)
       (map nquads/nquad->nquad-string)
       (string/join \newline)
       append-newline))

(defn ^:export howl-to-ntriples
  "Given one or more environments or HOWL strings,
   return a string of N-Triples (dropping the graph
   declaration if present)"
  [& args]
  (->> (apply howl-to-nquad-vectors args)
       (map nquads/nquad->ntriple-string)
       (string/join \newline)
       append-newline))

(defn ^:export nquads-to-howl
  "Given one or more environments or NQuads strings,
   return a string of N-Quads."
  [& args]
  (->> (apply parse-nquads-strings args)
       (map howl/update-parse-tree)
       howl/update-whitespace
       (map howl/block->howl-string)
       (string/join "")))
