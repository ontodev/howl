(ns howl.constants.owl
  (:require [howl.util :as util :refer [owl>]])
  (:refer-clojure :exclude [class]))

(def ^:const class (owl> "Class"))
(def ^:const restriction (owl> "Restriction"))
(def ^:const complement-of (owl> "complementOf"))
(def ^:const on-property (owl> "onProperty"))
(def ^:const some-values-from (owl> "someValuesFrom"))
(def ^:const all-values-from (owl> "allValuesFrom"))
