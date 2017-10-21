(ns clojure-erlastic.util
  (:import (java.nio.charset Charset)
           (sun.nio.cs UTF_8)))

;; CONFIGURATION
(def default-config {:str-detect :none
                     :convention :elixir
                     :str-autodetect-len 10})

(defn get-conf [config key]
  (get config key (key default-config)))

;; String conversion and detection utility functions
(def ^UTF_8 utf-8 "The UTF-8 charset object" (Charset/forName "UTF-8"))

(def printables #{Character/UPPERCASE_LETTER Character/TITLECASE_LETTER
                  Character/SPACE_SEPARATOR
                  Character/PARAGRAPH_SEPARATOR Character/LOWERCASE_LETTER
                  Character/CURRENCY_SYMBOL Character/DECIMAL_DIGIT_NUMBER
                  Character/DASH_PUNCTUATION Character/CONNECTOR_PUNCTUATION
                  Character/END_PUNCTUATION Character/FINAL_QUOTE_PUNCTUATION
                  Character/INITIAL_QUOTE_PUNCTUATION Character/START_PUNCTUATION
                  Character/OTHER_PUNCTUATION})
