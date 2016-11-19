(ns howl.core
  "Core HOWL definitions and operations."
  (:require [clojure.string :as string]))


;; # BLOCKS

; There are seven block types in HOWL:
; COMMENT, PREFIXES, LABELS, BASE, GRAPH, SUBJECT, STATEMENT
;
; The core of HOWL processing is the "block map",
; which contains enough information to convert into any supported format:
; HOWL, NQuads, JSON.
;
; We use a hub-and-spoke model with the block map representation in the middle,
; then provide methods for each conversion:
;
; - HOWL string to block
; - block to HOWL string
; - NQuad to block
; - block to NQuad
; - JSON string to block
; - block to JSON string
;
; We also have blocks to blocks transformations,
; such as normalization and sorting.



(defn reset-environment
  [env]
  (dissoc env :current-graph-iri :current-subject-iri))


