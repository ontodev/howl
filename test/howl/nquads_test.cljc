(ns howl.nquads-test
  "Test N-Quads rendering and supporting functions."
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [clojure.string :as string]

            [howl.nquads :as nq]
            [howl.core :as core]
            [howl.util :refer [rdf> owl>]]))

;; FIXME TODO
;; (deftest test-nquad-string->nquad
;;   (testing "parse"
;;     (are [x y] (= (nq/nquad-string->nquad x) y)
;;       "<s> <p> <o> ."            [nil "s" "p" "o" "LINK"]
;;       "<s> <p> <o> <g> ."        ["g" "s" "p" "o" "LINK"]
;;       "<s> <p> \"o\" ."          [nil "s" "p" "o" "PLAIN"]
;;       "<s> <p> \"o\"@en ."       [nil "s" "p" "o" "@en"]
;;       "<s> <p> \"o\"^^<d> ."     [nil "s" "p" "o" "d"]
;;       "<s> <p> \"o\"@en <g> ."   ["g" "s" "p" "o" "@en"]
;;       "<s> <p> \"o\"^^<d> <g> ." ["g" "s" "p" "o" "d"]
;;       "_:s <p> \"o\"@en <g> ."   ["g" "_:s" "p" "o" "@en"]
;;       "<s> <p> _:o <g> ."        ["g" "s" "p" "_:o" "LINK"])))

(deftest test-sequential-blank-nodes
  (testing "sequential"
    (is (= (nq/sequential-blank-nodes
            [["g" "_:bar" "p" "_:b1" "LINK"]
             ["g" "_:bar" "p" "_:literal" "PLAIN"]])
           [["g" "_:b1" "p" "_:b2" "LINK"]
            ["g" "_:b1" "p" "_:literal" "PLAIN"]]))))
