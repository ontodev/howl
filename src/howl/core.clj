(ns howl.core
  (:require [clojure.string :as string]))

(def test1 (slurp "test1.howl"))

; TODO: resolve-relative-iri
; TODO: resolve-prefixed-name
; TODO: resolve-label
; TODO: resolve-name

(defn add-prefix
  "Given a HOWL state and a PREFIX line,
   update and return the HOWL state."
  [state number line]
  (let [[_ prefix iri] (re-matches #"PREFIX\s+(\w+):\s+<(.*?)>\s*" line)]
    (when (find (:prefixes state) prefix)
      (throw
       (Exception.
        (format "PREFIX '%s' already defined at line %d" prefix number))))
    (assoc-in state [:prefixes prefix] iri)))

(defn reduce-howl
  "Given a HOWL collection and a line of HOWL,
   update the collection."
  [state number line]
  (try
    (cond 
      (string/blank? line)
      state
      ; BASE
      (.startsWith line "PREFIX ")
      (add-prefix state number line)
      ; LABEL
      ; GRAPH
      ; SUBJECT
      ; 
      :else
      (throw (Exception. (str "Unhandled line: " number " " line))))
    (catch Exception e
      (do (println state)
          (throw e)))))

(defn howl2n3
  "Convert a HOWL string to N3"
  [text]
  (->> text
       string/split-lines
       (reduce-kv reduce-howl {})))
