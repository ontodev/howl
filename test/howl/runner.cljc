(ns howl.runner
  (:require [doo.runner :refer [doo-tests]]
            [howl.core-test]))

#?(:cljs (doo-tests
          'howl.core-test))
