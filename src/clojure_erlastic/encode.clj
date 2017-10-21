(ns clojure-erlastic.encode
  (:require [clojure-erlastic.util :refer [get-conf printables utf-8 default-config]])
  (:import (com.ericsson.otp.erlang OtpErlangAtom OtpErlangObject OtpErlangTuple
                                    OtpErlangString OtpErlangList OtpErlangLong
                                    OtpErlangInt OtpErlangBinary OtpErlangDouble
                                    OtpErlangMap)))

(defn- erlang-objects
  #^"[Lcom.ericsson.otp.erlang.OtpErlangObject;"
    [xs]
  (into-array OtpErlangObject xs))

(defprotocol IErlang
  (-encode- [this config]))

(extend-type nil
  IErlang
  (-encode- [this config]
    (if (= (get-conf config :convention) :elixir)
      (OtpErlangAtom. "nil")
      (OtpErlangAtom. "undefined"))))

(extend-type Character
  IErlang
  (-encode- [this config]
    (-encode- (int this) config)))

(extend-type Long
  IErlang
  (-encode- [this config]
    (OtpErlangLong. this)))

(extend-type Integer
  IErlang
  (-encode- [this config]
    (OtpErlangInt. (long this))))

(extend-type clojure.lang.ISeq
  IErlang
  (-encode- [this config]
    (OtpErlangList. (erlang-objects (map #(-encode- % config) this)))))

(extend-type clojure.lang.Keyword
  IErlang
  (-encode- [this config]
    (OtpErlangAtom. (name this))))

(extend-type clojure.lang.IPersistentSet
  IErlang
  (-encode- [this config]
    (OtpErlangList. (erlang-objects (map #(-encode- % config) this)))))

(extend-type Float
  IErlang
  (-encode- [this config]
    (OtpErlangDouble. (double this))))

(extend-type Double
  IErlang
  (-encode- [this config]
    (OtpErlangDouble. (double this))))

(extend-type clojure.lang.IPersistentVector
  IErlang
  (-encode- [this config]
    (OtpErlangTuple. (erlang-objects (map #(-encode- % config) this)))))

(extend-type java.lang.String
  IErlang
  (-encode- [this config]
    (if (= (get-conf config :convention) :elixir)
      (OtpErlangBinary. (bytes (.getBytes ^String this "UTF-8")))
      (-encode- (seq this) config))))

(extend-type java.lang.Boolean
  IErlang
  (-encode- [this config]
    (-encode- (keyword (str this)) this)))

(extend-type clojure.lang.IPersistentMap
  IErlang
  (-encode- [this config]
    (let [c    (count this)
          ks   #^"[Lcom.ericsson.otp.erlang.OtpErlangObject;" (make-array OtpErlangObject c)
          vs   #^"[Lcom.ericsson.otp.erlang.OtpErlangObject;" (make-array OtpErlangObject c)]
      (reduce-kv
        (fn [i k v]
          (aset ks i (-encode- k config))
          (aset vs i (-encode- v config))
          (inc i))
        0
        this)
      (OtpErlangMap. ks vs))))

(extend-type (Class/forName "[B")
  IErlang
  (-encode- [this config]
    (OtpErlangBinary. (bytes this))))

(extend-type Object
  IErlang
  (-encode- [this config]
    (-encode- (str this) config)))

