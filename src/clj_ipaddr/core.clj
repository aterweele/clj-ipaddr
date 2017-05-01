(ns clj-ipaddr.core
  ;; TODO: clean up any unused imports
  (:import [clojure.lang BigInt]
           [com.google.common.primitives Ints]
           [java.math BigInteger]
           [java.net InetAddress Inet4Address Inet6Address]))

(defprotocol IPAddressRepresentation
  (ip-address [this] "Yields a java.net.InetAddress of the provided value."))

;; byte array. Does not work inside the extend-protocol below.
(extend-type (Class/forName "[B")
  IPAddressRepresentation
  (ip-address [bs] (InetAddress/getByAddress bs)))

(extend-protocol IPAddressRepresentation
  String
  (ip-address [s] (InetAddress/getByName s)))

;; TODO: consider removing these two. Cute but perhaps not useful.
(defprotocol IPAddress
  (version [this]))

(extend-protocol IPAddress
  Inet4Address
  (version [_] #_::ipv4 4)
  Inet6Address
  (version [_] #_::ipv6 6))

;; TODO: write the network-making stuff.

;; keep in mind: (1) clojure numbers are naturally longs, not
;; ints. (2) BigInt might be a better choice than BigInteger. I even
;; might be able to get away with using only BigInts if
;; implementation-wise, they fall back to being primitives (in our
;; case, ints) when the values are small.
