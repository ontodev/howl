(ns howl.constants.owl
  (:require [howl.util :as util])
  (:refer-clojure :exclude [class]))

(defn owl> [name] (str "http://www.w3.org/2002/07/owl#" name))

(def ^:const class (owl> "Class"))
(def ^:const restriction (owl> "Restriction"))
(def ^:const complement-of (owl> "complementOf"))
(def ^:const on-property (owl> "onProperty"))
(def ^:const some-values-from (owl> "someValuesFrom"))
(def ^:const all-values-from (owl> "allValuesFrom"))
