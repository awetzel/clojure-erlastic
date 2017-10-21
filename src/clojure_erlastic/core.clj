(ns clojure-erlastic.core
  (:require [clojure.core.async :as async :refer [<! >! <!! chan go close!]]
            [clojure-erlastic.decode :as d]
            [clojure-erlastic.encode :as e]
            [clojure-erlastic.util :refer [default-config get-conf]])
  (:import (com.ericsson.otp.erlang OtpErlangObject OtpErlangDecodeException
                                    OtpInputStream OtpOutputStream OtpException)))

;; Codec functions
(def decode d/-decode-)
(def encode e/-encode-)

;; Erlang port communication on stderr - stdin
(defn log [& params]
  (.println System/err (apply str params)))

(defn port-connection
  ([] (port-connection default-config))
  ([config]
   (let [in (chan) out (chan)]
     (go ;; term receiver coroutine
       (try
         (while true
           (let [len-buf (byte-array 4)]
             (.read System/in len-buf)
             (let [term-len (.read4BE (OtpInputStream. len-buf))
                   term-buf (byte-array term-len)]
               (.read System/in term-buf)
               (let [b (decode (.read_any (OtpInputStream. term-buf)) config)]
                 (>! in b)))))
         (catch Exception e
           (do
             (log "receive error : " (type e) " " (.getMessage ^Exception e))
             (close! out)
             (close! in)))))
     (go ;; term sender coroutine
       (loop []
         (when-let [term (<! out)]
           (try
             (let [out-term (OtpOutputStream. ^OtpErlangObject (encode term config))]
               (doto (OtpOutputStream.)
                 (.write4BE (+ 1 (.size out-term)))
                 (.write 131)
                 (.writeTo System/out))
               (.writeTo out-term System/out)
               (.flush System/out))
             (catch Exception e (log "send error : " (type e) " " (.getMessage e))))
           (recur))))
     [in out])))

;; erlang style genserver management
(defn run-server
  ([handle] (run-server (fn [state] state) handle default-config))
  ([init handle] (run-server init handle default-config))
  ([init handle config]
   (let [[in out] (port-connection config)]
     (<!! (go
            (loop [state (init (<! in))]
              (if-let [req (<! in)]
                (let [res (try
                            (handle req state)
                            (catch Exception e
                              [:stop [:error e]]))]
                  (case (res 0)
                    :reply (do
                             (>! out (res 1))
                             (recur (res 2)))
                    :noreply (recur (res 1))
                    :stop (res 1)))
                :normal)))))))
