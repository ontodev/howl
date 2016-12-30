(ns howl.runner
  (:require [doo.runner :refer-macros [doo-all-tests]]
            [howl.core-test]
            [howl.howl-test]
            [howl.link-test]
            [howl.manchester-test]
            [howl.nquads-test]
            [howl.util-test]))

#?(:cljs (doo-all-tests))
