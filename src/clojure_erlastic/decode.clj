(ns clojure-erlastic.decode
  (:require [clojure-erlastic.util :refer [get-conf printables utf-8]])
  (:import (com.ericsson.otp.erlang OtpErlangAtom OtpErlangObject OtpErlangTuple
                                    OtpErlangString OtpErlangList OtpErlangLong
                                    OtpErlangBinary OtpErlangDouble OtpErlangMap
                                    OtpErlangInt)
           (java.nio ByteBuffer CharBuffer)))

(defn str-or-bin [^bytes bin]
  (try
    (String. bin utf-8)
    (catch Exception _
      bin)))

(defn str-or-lst [xs]
  (try
    (apply str (map char xs))
    (catch Exception _
      xs)))

(defn- printable-long? [^Long c]
  (contains? printables (Character/getType c)))

(defn seq-str? [xs detect-len]
  (every? #(and (Character/isValidCodePoint %)
                (printable-long? %))
          (take detect-len xs)))

(defn- printable? [^Character c]
  (contains? printables (Character/getType c)))

(defn bin-str? [^bytes bin detect-len]
  (let [char-res  (CharBuffer/allocate detect-len)
        len       (min (alength bin) detect-len)
        coder-res (.decode (.newDecoder utf-8) (ByteBuffer/wrap bin 0 len) char-res (= len (alength bin)))
        char-len  (.position char-res)]
    (doto char-res
      (.rewind)
      (.limit char-len))
    (and (not (.isError coder-res))
         (every? printable? (.toString char-res)))))

(defprotocol IErlang
  (-decode- [this config]))

(extend-type OtpErlangAtom
  IErlang
  (-decode- [obj config]
    (let [k           (keyword (.atomValue obj))
          convention  (get-conf config :convention)]
      (cond
        (#{:true :false} k)
        (= k :true)

        (and (= convention :elixir)
             (= k :nil))
        nil

        (and (= convention :erlang)
             (= k :undefined))
        nil

        :else k))))

(extend-type OtpErlangList
  IErlang
  (-decode- [obj config]
    (let [elem-seq    (seq (map #(-decode- % config) (.elements obj)))
          erlang?     (= (get-conf config :convention) :erlang)
          str-detect  (get-conf config :str-detect)]
      (cond
        (and erlang? (= str-detect :all))
        (str-or-lst elem-seq)

        (and erlang? (= str-detect :auto))
        (if (seq-str? elem-seq (get-conf config :str-autodetect-len))
          (str-or-lst elem-seq)
          elem-seq)

        :else elem-seq))))

(extend-type OtpErlangTuple
  IErlang
  (-decode- [obj config]
    (mapv #(-decode- % config)
          (.elements obj))))

(extend-type OtpErlangString
  IErlang
  (-decode- [obj config]
    (.stringValue obj)))

(extend-type OtpErlangBinary
  IErlang
  (-decode- [obj config]
    (let [bin        (.binaryValue obj)
          elixir?    (= (get-conf config :convention) :elixir)
          str-detect (get-conf config :str-detect)]
      (cond
        (and elixir? (= str-detect :all))
        (str-or-bin bin)

        (and elixir? (= str-detect :auto))
        (if (bin-str? bin (get-conf config :str-autodetect-len))
          (str-or-bin bin)
          bin)

        :else bin))))

(extend-type OtpErlangMap
  IErlang
  (-decode- [obj config]
    (reduce (fn [acc ^java.util.Map$Entry x]
              (assoc acc (-decode- (.getKey x) config) (-decode- (.getValue x) config)))
            {}
            (.entrySet obj))))

(extend-type OtpErlangLong
  IErlang
  (-decode- [obj config]
    (.longValue obj)))

(extend-type OtpErlangInt
  IErlang
  (-decode- [obj config]
    (.intValue obj)))

(extend-type OtpErlangDouble
  IErlang
  (-decode- [obj config]
    (.doubleValue obj)))

(extend-type Object
  IErlang
  (-decode- [obj config]
    :decoding_error))
