(ns howl.cli
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [howl.util :as util]
            [howl.howl :as howl]
            [howl.nquads :as nquads]
            [howl.api :as api])
  (:gen-class))

;; # CLI
;;
;; The HOWL command line interface is modelled on Pandoc.
;; Input files are converted to a sequence of block maps,
;; updating the environment as we go,
;; then output the block maps to files.
;; Some inputs are "context":
;; they update the environment by do nout output blocks.
;;
;; We support streaming as much as possible,
;; and especially with the default HOWL to NQuads conversion.
;; Streams of HOWL input lines are converted to block maps,
;; rendered to NQuad strings, and output.
;; To accomplish this we use an "inside out" callback style.
;; First we build a function for outputting strings,
;; then wrap them with functions to render blocks to strings,
;; and pass that to the functions that handle each input in turn.

;; # Formats
;;
;; We currently support HOWL, NQuads, and NTriples
;; for input and output.

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

;; # Parsing
;;
;; We use some pure functions to turn the arguments
;; into a sensible datastructure.

(defmulti handle-flag
  "Given command line args with flags, handles the given flag"
  (fn [coll arg file-format] (coll :flag)))

(defmethod handle-flag :default [coll arg file-format]
  (update-in coll [:errors] conj (str "Unrecognized flag: " (coll :flag))))

(defmethod handle-flag :context [coll arg file-format]
  (let [coll (dissoc coll :flag)]
    (if file-format
      (update-in
       coll
       [:inputs]
       conj
       {:path arg :format file-format :context true})
      (update-in
       coll
       [:errors]
       conj
       (str "Unhandled context file format: " arg)))))

(defmethod handle-flag :output [coll arg file-format]
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
       (str "Unhandled output file format: " arg)))))

(defn parse-arg
  "Given a configuration map and an argument string,
   return the updated configuration map."
  [coll raw-arg]
  (let [flag (:flag coll)
        arg (string/trim raw-arg)
        file-format (detect-format arg)]
    (cond
      ; TODO: handle STDIN/STDOUT properly
      (= arg "-")
      (update-in coll [:errors] conj (str "Unhandled argument: " arg))

      flag (handle-flag coll arg file-format)

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
       {:path arg :format file-format})

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

(defn add-defaults
  "Given a config map, add any required defaults,
   such as the default NQuads output to STDOUT."
  [{:keys [outputs] :as config}]
  (if (seq outputs)
    config
    (assoc config :outputs [{:path "-" :format :nquads}])))

;; # Process Input
;;
;; We process each input file into a sequence of block maps,
;; then call an output function on each block.
;; Context files do not generate blocks,
;; the just update the environment map.
;; NQuads/NTriples input files do not update the environment,
;; they just output blocks.
;; So use an NQuads file as a context does nothing.

(defn process-howl-input!
  "Given an environment map, an input map for a HOWL file,
   and a block processing function,
   read the input as a lazy sequence of lines,
   and process them into blocks,
   calling the processing function,
   and return the updated environment."
  [env {:keys [path] :as input} process-block!]
  (with-open [reader (io/reader path)]
    (howl/process-lines! env (line-seq reader) process-block!)))

(defn process-nquads-input!
  "Given an environment map, an input map for an NQuads file,
   and a block-processing function,
   process the NQuads file,
   calling the block processing function on each block.
   Return the environment map unchanged."
  [env {:keys [context path] :as input} process-block!]
  (when-not context
    (with-open [reader (io/reader path)]
      (doseq [block (nquads/lines->blocks env (line-seq reader))]
        (process-block! block))))
  env)

(def input-function
  {:howl     process-howl-input!
   :nquads   process-nquads-input!
   :ntriples process-nquads-input!})

(defn process-input!
  "Given an output function, an environment map, an input description,
   handle the input using the output function,
   and return the updated environment.
   If the input is a context then there is no output,
   so replace the output function with a no-op."
  [output-fn! env {:keys [format context] :as input}]
  (if-let [input-fn! (get input-function format)]
    (input-fn! env input (if context :no-op output-fn!))
    (throw
     (new Exception (str "Unrecognized file format: " format)))))

;; # Render Output
;;
;; The render functions take a block and render it to a string,
;; then call the output function on the string.

(defn render-howl-output!
  [options block output-fn!]
  (output-fn! (howl/block->howl-string block)))

(defn render-nquads-output!
  [options block output-fn!]
  (doseq [nquad (nquads/block->nquads block)]
    (output-fn! (nquads/nquad->nquad-string nquad))))

(defn render-ntriples-output!
  [options block output-fn!]
  (doseq [nquad (nquads/block->nquads block)]
    (output-fn! (nquads/nquad->ntriple-string nquad))))

(def output-function
  {:howl     render-howl-output!
   :nquads   render-nquads-output!
   :ntriples render-ntriples-output!})

(defn build-output-fn
  "Given a config map with one or more output descriptions,
   return a function that accepts a block and handles the output(s)."
  [{:keys [options outputs] :as config}]
  (fn [block]
    (doseq [{:keys [writer render-fn!] :as output} outputs]
      (render-fn!
       options
       block
       (if writer #(.write writer (str % "\n")) println)))))

;; # Output
;;
;; We allow one or more outputs.
;; Each output has its own format.
;; The default is to ouput NQuads to STDOUT.
;; We open all the required output files,
;; build an output function that renders blocks and writes to each output,
;; then handle each input
;; and finally close each output file.

(defn open-outputs!
  "Given a config map with zero or more :outputs maps,
   open a writer for each output,
   and return the updated config map."
  [{:keys [outputs] :as config}]
  (assoc
   config
   :outputs
   (doall
    (for [{:keys [path format] :as output} outputs]
      (if-let [render-fn! (get output-function format)]
        (assoc
         output
         :render-fn! render-fn!
         :writer (when-not (= "-" path) (io/writer path)))
        (throw
         (new Exception (str "Unrecognized file format: " format))))))))

(defn close-outputs!
  "Given a config map with zero or more :outputs maps,
   close the writer for each output,
   and return the updated config map."
  [{:keys [outputs] :as config}]
  (assoc
   config
   :outputs
   (doall
    (for [{:keys [writer] :as output} outputs]
      (do
        (when writer (.close writer))
        (dissoc output :writer))))))

(defn process!
  "Given a config map,
   open required files,
   build an output function that handles each output,
   then process each input (accumulating the environment),
   finally closing the opened files."
  [config]
  (let [config (open-outputs! config)]
    (reduce
     (partial process-input! (build-output-fn config))
     {:config config}
     (:inputs config))
    (close-outputs! config)))

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

(defn handle-args!
  "Given a sequence of argument strings, handle the arguments,
   then return the pair of a status code and a message (or nil)."
  [args]
  (let [{:keys [options errors] :as config} (parse-args args)]
    (cond
      (:help options) [0 usage]
      (:version options) [0 (version)]
      (seq errors) [1 (error-msg errors)]
      :else
      (try
        (process! (add-defaults config))
        [0 nil]
        (catch Exception e
          (.printStackTrace e)
          [1 (.getMessage e)])))))

(defn -main
  "Handle arguments, print any messages, then exit with a status code."
  [& args]
  (let [[status message] (handle-args! args)]
    (when message (println message))
    (System/exit status)))
