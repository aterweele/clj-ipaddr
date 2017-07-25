(ns clj-ipaddr.util.byte-array
  (:refer-clojure :exclude [bit-xor bit-not bit-or bit-and])
  (:require [clojure.core :as core]))

(defn- count-unset-byte
  [b]
  (loop [i 0 count 0]
    (if (> i 7)
      count
      (recur (inc i) (if (bit-test b i) count (inc count))))))

(defn count-unset
  [bs]
  (->> bs
       (map count-unset-byte)
       (reduce + 0)))

(def bit-xor (comp byte-array (partial map core/bit-xor)))
(def bit-or (comp byte-array (partial map core/bit-or)))
(def bit-and (comp byte-array (partial map core/bit-and)))
(def bit-not (comp byte-array (partial map core/bit-not)))

;; FIXME: I think this produces basically the right masks but it makes
;; duplicates which is no good.
(defn all-masks
  ([subnet-mask] (all-masks (vec subnet-mask) 0))
  ([subnet-mask i]
   (let [byte-index (- (-> subnet-mask count dec) (quot i 8))
         bit-index  (mod i 8)]
     (if-not (neg? byte-index)
       (let [byte (nth subnet-mask byte-index)]
         (if (bit-test byte bit-index)
           (all-masks subnet-mask (inc i))
           (lazy-cat
            ;; continue with bit unset
            (all-masks subnet-mask (inc i))
            ;; continue with bit set
            (all-masks (assoc subnet-mask
                              byte-index
                              (bit-set byte bit-index))
                       (inc i)))))
       [subnet-mask]))))

(defn simple-mask
  "Yields a mask of size bytes with the first n bits set"
  [size n]
  ;; TODO: implement, possibly change arguments
  )
