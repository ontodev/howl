(ns howl.api-test
  "Integration tests against API."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [howl.link :as link]
            [howl.api :as api]))

;(println (howl-to-nquads (slurp "test/empty-context/test1.howl")))
;(println (nquads-to-howl (slurp "test/nquads/test1.nq")))
;(println
; (nquads-to-howl
;  (howl-to-environment (slurp "test/typed-context/context.howl"))
;  (slurp "test/nquads/test1.nq")))
;(println
; (;parse-howl-strings
;  howl-to-nquads
;  (howl-to-environment (slurp "test/format-context/context.howl"))
;  (slurp "test/format-context/list1.howl")))
;(println
; (nquads-to-howl
;  ;parse-nquads-strings
;  (howl-to-environment (slurp "test/format-context/context.howl"))
;  (slurp "test/nquads/list1.nq")))

(def options {:options {:sequential-blank-nodes true}})

(defn compare-howl->nquads
  [context-path howl-path nquads-path]
  (testing (string/join " " [context-path howl-path nquads-path])
    (let [context (when context-path (slurp context-path))
          howl    (slurp howl-path)
          nquads  (slurp nquads-path)]
      (is (= nquads (api/howl-to-nquads options context howl))))))

(deftest test-howl->nquads
  (compare-howl->nquads
   "test/format-context/context.howl"
   "test/format-context/manchester1.howl"
   "test/nquads/manchester1.nq"))

(defn compare-howl<->nquads
  [context-path howl-path nquads-path]
  (testing (string/join " " [context-path howl-path nquads-path])
    (let [context (when context-path (slurp context-path))
          howl    (slurp howl-path)
          nquads  (slurp nquads-path)]
      (is (= nquads (api/howl-to-nquads options context howl)))
      (is (= howl (api/nquads-to-howl options (api/howl-to-environment context) nquads))))))

;; (println
;;  (api/nquads-to-howl
;;   {:options {:sequential-blank-nodes true}}
;;   (howl.table/tsv-to-environment
;;    (api/howl-to-environment
;;     (slurp "test/obi/context.howl"))
;;    (io/reader "test/obi/terms.tsv"))
;;   (slurp "test/obi/obi_core.nt")))

;; (println
;;  (api/howl-to-nquads
;;   {:options {:sequential-blank-nodes true}}
;;   (howl.table/process-lines!
;;    (api/howl-to-environment
;;     (slurp "test/obi/context.howl"))
;;    (line-seq (io/reader "test/obi/terms.tsv"))
;;    :no-op)
;;   (slurp "test/obi/obi-stack-overflow.howl")))

;; (do
;;   (pprint (vec (get-top-fperf 15)))
;;   (println " ")
;;   (pprint (vec (get-top-fperf 15 :call-count)))
;;   nil)

;; (println
;;  (api/howl-to-nquads
;;   {:options {:sequential-blank-nodes true}}
;;   (howl.table/tsv-to-environment
;;    (api/howl-to-environment
;;     (slurp "test/obi/context.howl"))
;;    (io/reader "test/obi/terms.tsv"))
;;   (slurp "test/obi/obi.howl")))

;; (println
;;  (api/nquads-to-howl
;;   {:options {:sequential-blank-nodes true}}
;;   (api/howl-to-environment (slurp "test/format-context/context.howl"))
;;   (slurp "test/nquads/manchester1.nq")))

(deftest test-howl<->nquads
  (compare-howl<->nquads
   nil
   "test/empty-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl<->nquads
   "test/untyped-context/context.howl"
   "test/untyped-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl<->nquads
   "test/typed-context/context.howl"
   "test/typed-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl<->nquads
   "test/format-context/context.howl"
   "test/format-context/test1.howl"
   "test/nquads/test1.nq")
  (compare-howl<->nquads
   "test/format-context/context.howl"
   "test/format-context/manchester1.howl"
   "test/nquads/manchester1.nq")
  (compare-howl<->nquads
   "test/format-context/context.howl"
   "test/format-context/manchester2.howl"
   "test/nquads/manchester2.nq")
  (compare-howl<->nquads
   "test/laika-context/context.howl"
   "test/laika-context/test1.howl"
   "test/laika-context/test1.nq")
  (compare-howl<->nquads
   "test/untyped-context/context.howl"
   "test/untyped-context/annotations1.howl"
   "test/nquads/annotations1.nq")
  (compare-howl<->nquads
   "test/format-context/context.howl"
   "test/format-context/list1.howl"
   "test/nquads/list1.nq"))

(defn round-trip
  [context-path howl-path]
  (testing (string/join " " [context-path howl-path])
    (let [context (when context-path (slurp context-path))
          howl    (slurp howl-path)
          nquads  (api/howl-to-nquads context howl)]
      (is (= howl (api/nquads-to-howl (api/howl-to-environment context) nquads))))))

(deftest test-howl-to-nquads-roundtrip
  (round-trip
   nil
   "test/empty-context/test1.howl")
  (round-trip
   "test/untyped-context/context.howl"
   "test/untyped-context/test1.howl")
  (round-trip
   "test/typed-context/context.howl"
   "test/typed-context/test1.howl")
  (round-trip
   "test/format-context/context.howl"
   "test/format-context/test1.howl")
  (round-trip
   "test/untyped-context/context.howl"
   "test/untyped-context/annotations1.howl")
  (round-trip
   "test/format-context/context.howl"
   "test/format-context/test1.howl")
  (round-trip
   "test/format-context/context.howl"
   "test/format-context/list1.howl"))
