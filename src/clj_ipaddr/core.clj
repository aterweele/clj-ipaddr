(ns clj-ipaddr.core
  (:refer-clojure :exclude [bytes])
  (:require [clj-ipaddr.util.byte-array :as util]
            [clojure.math.numeric-tower :as math])
  (:import [clojure.lang IPersistentSet ILookup]
           [java.io Writer]
           [java.net InetAddress Inet4Address Inet6Address]))

(defprotocol IPAddressRepresentation
  (ip-address [this] "Yields a java.net.InetAddress of the provided value."))

;; byte array. Does not work inside the extend-protocol below.
(extend-type (Class/forName "[B")
  IPAddressRepresentation
  (ip-address [bs] (InetAddress/getByAddress bs)))
(derive (Class/forName "[B") `IPAddressRepresentation)

;; TODO: check the derivations above and below. Are they still
;; necessary? should I make a separate hierarchy? Polluting the global
;; hierarchy could be considered rude.

(extend-protocol IPAddressRepresentation
  String
  (ip-address [s] (InetAddress/getByName s)))
(derive String `IPAddressRepresentation)

(defprotocol IPAddress
  (version [this])
  (bytes [this]))

(extend-protocol IPAddress
  Inet4Address
  (version [_] 4)
  (bytes [this] (.getAddress this))
  Inet6Address
  (version [_] 6)
  (bytes [this] (.getAddress this)))

(deftype IPNetwork
    [seed mask]
  ILookup
  (valAt [_ key] ({:seed seed :mask mask} key))
  (valAt [_ key not-found] ({:seed seed :mask mask} key not-found))
  IPersistentSet
  (count [_]
    (->> mask
         .getAddress
         util/count-unset
         (math/expt 2)))
  (contains [_ ip]
    (let [[ip seed mask] (map bytes [ip seed mask])]
      (-> (util/bit-or
           ;; either the seed and the ip agree...
           (util/bit-not (util/bit-xor seed ip))
           ;; ...or the mask permits any value
           (util/bit-not mask))
          util/count-unset
          zero?)))
  ;; FIXME: not giving the correct
  ;; order. (seq (ip-network "192.168.0.0" "255.255.255.245")) is a
  ;; failing test case.
  (seq [_]
    (let [seed       (bytes seed)
          orig-mask  (bytes mask)
          apply-mask (fn [mask]
                       (util/bit-or
                        seed
                        (util/bit-and mask (util/bit-not orig-mask))))
          mask-xf (comp (map apply-mask)
                        (map ip-address))]
      ;; TODO: a sorted set is best here but I have not
      ;; got (into (sorted-set)) working
      (sequence mask-xf (util/all-masks orig-mask))))
  (empty [_] (sorted-set))
  (equiv [_ {other-seed :seed other-mask :mask}]
    ;; TODO: check
    (and (= seed other-seed) (= mask other-mask))))
(alter-meta! #'->IPNetwork #(assoc % :private true))
;; FIXME: it is still printing as a set.
(defmethod print-method IPNetwork [{:keys [seed mask] :as network} ^Writer w]
  (.write w (format "<IP network broadcast: %s mask: %s addresses: %s>"
                    seed mask (count network))))

;; TODO: move to a utils file?
(defn- unset-masked-bits
  "Unset every bit in ip that is unset in mask."
  [ip mask]
  (ip-address (util/bit-and (bytes mask) (bytes ip))))

(defmulti ip-network (fn [& xs] (mapv class xs)))
;; TODO: need to make sure that every unset bit in the mask is unset
;; in the seed. I have implemented it but it needs testing.
(defmethod ip-network [Inet4Address Inet4Address] [seed mask]
  (->IPNetwork (unset-masked-bits seed mask) mask))
(defmethod ip-network [Inet6Address Inet6Address] [seed mask]
  (->IPNetwork (unset-masked-bits seed mask) mask))
(defmethod ip-network
  [`IPAddressRepresentation `IPAddressRepresentation]
  [seed mask]
  (ip-network (ip-address seed) (ip-address mask)))
;; TODO: arity-1 which takes "x.x.x.x/x"
