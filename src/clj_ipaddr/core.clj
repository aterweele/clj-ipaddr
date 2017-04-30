(ns clj-ipaddr.core
  (:import [clojure.lang BigInt]
           [com.google.common.primitives Ints]
           [java.math BigInteger]
           [java.net InetAddress]))

;; TODO: the dispatch is purely on type, so a defprotocol and
;; extend-protocol might pattern be a better fit here.
(defmulti ip-address
  "Yields a java.net.InetAddress of the provided value."
  class)
(defmethod ip-address String [s]
  (InetAddress/getByName s))
(defmethod ip-address
  ;; byte array
  (Class/forName "[B") [bs]
  (InetAddress/getByAddress bs))
(defmethod ip-address Integer [i]
  (-> i Ints/toByteArray ip-address))
;; TODO: this is very opinionated. Anything bigger than an Integer is
;; too long to be an IPv4 address and is therefore made into an IPv6
;; address. OTOH I do not want to ignore the information in the
;; types. Another approach might do that.
(defmethod ip-address BigInteger [i]
  (let [ipv6-addr-size 16
        bs (.toByteArray i)
        l (count bs)]
    ;; FIXME: this 0-padding scheme is janky and probably does
    ;; something unexpected with numbers bigger than 16
    ;; bytes (expected behavior for >16 bytes: make a byte array that
    ;; is too long and fail when trying to make an IPv6 address out of
    ;; that.)

    ;; I am guessing that the actual behavior on such numbers will be
    ;; that they are truncated to a multiple of 20 bytes and are not
    ;; padded.

    ;; thinking about it more though I think the behavior on big
    ;; numbers might be right.
    (-> (partition ipv6-addr-size
                   ipv6-addr-size
                   bs
                   (repeat (- ipv6-addr-size l) 0))
        ;; tbh this is the jankiest bit. the fact that this is trying
        ;; to be a pad, but that it has to take this extra flattening
        ;; step...no good
        flatten
        byte-array
        ip-address)))
(defmethod ip-address BigInt [i]
  ;; TODO: I think it is right but I cannot find the docs for BigInt
  ;; so I am not sure of the semantics of .toBigInteger

  ;; avoidable by making BigInt be in charge of the padding (see
  ;; above) and BigInteger defer to BigInt.
  (-> i .toBigInteger ip-address))
(defmethod ip-address Long [l]
  (-> l bigint ip-address))

;; TODO: write the network-making stuff.

;; keep in mind: (1) clojure numbers are naturally longs, not
;; ints. (2) BigInt might be a better choice than BigInteger. I even
;; might be able to get away with using only BigInts if
;; implementation-wise, they fall back to being primitives (in our
;; case, ints) when the values are small.
