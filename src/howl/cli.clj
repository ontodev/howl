(ns howl.cli
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [howl.util :as util]
            [howl.api :as api])
  (:gen-class))

(def formats
  {"howl" :howl
   "nt"   :ntriples
   "nq"   :nquads})

(defn detect-format
  "Given a filepath, use the file extension
   to determine its format."
  [path]
  (let [[_ extension]
        (re-matches #".*\.(.*)$" (string/lower-case path))]
    (get formats extension)))

(defn parse-arg
  "Given a configuration map and an argument string,
   return the updated configuration map."
  [coll raw-arg]
  (let [flag (:flag coll)
        arg (string/trim raw-arg)
        file-format (detect-format arg)]
    (cond
      ; TODO: handle STDIN/STDOUT
      (= arg "-")
      (update-in coll [:errors] conj (str "Unhandled argument: " arg))

      ; Handle flagged arguments: context, output
      (= flag :context)
      (let [coll (dissoc coll :flag)]
        (if file-format
          (update-in
           coll
           [:inputs]
           conj
           {:path arg :format file-format :output false})
          (update-in
           coll
           [:errors]
           conj
           (str "Unhandled context file format: " arg))))

      (= flag :output)
      (let [coll (dissoc coll :flag)]
        (if file-format
          (update-in
           coll
           [:outputs]
           conj
           {:path arg :format file-format})
          (update-in
           coll
           [:errors]
           conj
           (str "Unhandled output file format: " arg))))

      flag
      (update-in coll [:errors] conj (str "Unrecognized flag: " flag))

      ; Handle options
      (contains? #{"-c" "--context"} arg)
      (assoc coll :flag :context)

      (contains? #{"-o" "--ouput"} arg)
      (assoc coll :flag :output)

      (contains? #{"-V" "--version"} arg)
      (assoc-in coll [:options :version] true)

      (contains? #{"-h" "--help"} arg)
      (assoc-in coll [:options :help] true)

      ; Handle arguments without options: basic inputs
      (.startsWith arg "-")
      (update-in coll [:errors] conj (str "Unrecognized option: " arg))

      file-format
      (update-in
       coll
       [:inputs]
       conj
       {:path arg :format file-format :output true})

      ; Unhandled arguments
      :else
      (update-in coll [:errors] conj (str "Unhandled argument: " arg)))))

(defn parse-args
  "Given a sequence of argument strings,
   return a map representing options, inputs, and outputs."
  [args]
  (reduce
   parse-arg
   {:arguments args
    :options {}
    :errors []
    :inputs []
    :outputs []}
   args))

(def usage
  "howl [OPTIONS] [FILES]
Input formats:  howl, nquads, ntriples
Output formats: howl, nquads, ntriples
Options:
  -c FILENAME   --context FILENAME
  -o FILENAME   --output FILENAME
  -V            --version
  -h            --help

WARN: This is an early development version.
Please see https://github.com/ontodev/howl for more information.")

(defn version
  "Return the version of this code."
  []
  (-> (eval 'howl.cli)
      .getPackage
      .getImplementationVersion
      (or "DEVELOPMENT")))

(defn error-msg
  "Given a sequence of error strings, return a single error string."
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn handle-args
  "Given a sequence of argument strings, handle the arguments,
   then return the pair of a status code and a message (or nil)."
  [args]
  (let [{:keys [options arguments errors inputs outputs] :as parsed}
        (parse-args args)]
    (cond
      (:help options) [0 usage]
      (:version options) [0 (version)]
      errors [1 (error-msg errors)]
      :else
      (try
        (println parsed) ; TODO: actually handle arguments
        [0 nil]
        (catch Exception e
          [1 "ERROR"])))))

(defn -main
  "Handle arguments, print any messages, then exit with a status code."
  [& args]
  (let [[status message] (handle-args args)]
    (when message (println message))
    (System/exit status)))
