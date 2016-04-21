(ns howl.util
  "Cross-platform utility functions."
  (:require
   [clojure.string :as string]
   #?@(:cljs
       [[goog.string]
        [goog.string.format]]))
  #?(:clj
     (:refer-clojure :exclude [format])))

#?(:clj
   (def format #'clojure.core/format)
   :cljs
   (defn format
     "Formats a string using goog.string.format."
     [fmt & args]
     (apply goog.string/format fmt args)))

(defn substring?
  "True if needle is a substring of haystack."
  [haystack needle]
  #?(:clj
     (.contains haystack needle)
     :cljs
     (> (.indexOf haystack needle) -1)))

(defn throw-exception
  "Given a sequence of arguments,
   throw a cross-platform exception."
  [& messages]
  (throw
   (#?(:clj Exception. :cljs js/Error.)
    (->> messages (map str) (string/join " ")))))
