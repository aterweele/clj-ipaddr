(ns clj-ipaddr.core
  (:import [com.google.common.primitives Ints]
           [java.math BigInteger]
           [java.net InetAddress]))

(defmulti ip-address #_type class)
(defmethod ip-address java.lang.String [s]
  (InetAddress/getByName s))
(defmethod ip-address
  ;; byte array
  (Class/forName "[B") [bs]
  (InetAddress/getByAddress bs))
(defmethod ip-address Integer [i]
  (-> i Ints/toByteArray ip-address))
;; FIXME: the byte array here can come out to be any length, so we
;; need to pad. If we pad, that means we could also handle Longs by
;; assuming that they are IPv6 addrs.
(defmethod ip-address BigInteger [i]
  (-> i .toByteArray InetAddress/getByAddress))
