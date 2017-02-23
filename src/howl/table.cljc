(ns howl.table
  (:require [clojure.string :as string]
            [howl.core :as core]
            [howl.howl :as howl]))

; TODO: generalize to CSV, maybe SQL, maybe Excel
; TODO: generalize beyond subject and statement blocks

(defn process-cell
  "Given an environment, an header string, and a cell string,
   process as a HOWL string,
   and return a block map."
  [env header cell]
  (howl/process-block
   env
   (if (= header "SUBJECT") cell (str header ": " cell))))

(defn process-lines!
  "Given an environment, a sequence of tab-separated lines,
   and a block processing function,
   reduce over each row and each cell to generate a block,
   calling the block processing function on each block,
   and returning the updated environment."
  [{:keys [source] :or {source "interactive"} :as env}
   lines
   process-block!]
  (let [headers (string/split (first lines) #"\t")]
    (reduce
     (fn [env row]
       (reduce
        (fn [env [header cell]]
          (let [block (process-cell env header cell)]
            (process-block! block)
            (-> (core/update-environment env block)
                (update-in [:line] + (count lines)))))
        env
        (map vector headers (string/split row #"\t"))))
     (assoc env :source source :line 2)
     (rest lines))))
