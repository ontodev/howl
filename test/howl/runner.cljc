(ns howl.runner
  (:require [doo.runner :refer-macros [doo-all-tests]]
            [howl.core-test]))

#?(:cljs (doo-all-tests))
