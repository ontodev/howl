(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require [clojure.test :refer :all]
            [clojure.string :as string]

            [howl.nquads :refer :all]
            [howl.core :as core]
            [howl.util :refer [rdf> owl>]]))

(deftest test-nquad-string->nquad
  (testing "parse"
    (are [x y] (= (nquad-string->nquad x) y)
      "<s> <p> <o> ."            [nil "s" "p" "o" "LINK"]
      "<s> <p> <o> <g> ."        ["g" "s" "p" "o" "LINK"]
      "<s> <p> \"o\" ."          [nil "s" "p" "o" "PLAIN"]
      "<s> <p> \"o\"@en ."       [nil "s" "p" "o" "@en"]
      "<s> <p> \"o\"^^<d> ."     [nil "s" "p" "o" "d"]
      "<s> <p> \"o\"@en <g> ."   ["g" "s" "p" "o" "@en"]
      "<s> <p> \"o\"^^<d> <g> ." ["g" "s" "p" "o" "d"]
      "_:s <p> \"o\"@en <g> ."   ["g" "_:s" "p" "o" "@en"]
      "<s> <p> _:o <g> ."        ["g" "s" "p" "_:o" "LINK"])))
