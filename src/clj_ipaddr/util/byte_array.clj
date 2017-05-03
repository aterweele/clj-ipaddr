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

;; TODO: finish
(defn all-masks
  ([subnet-mask] (all-masks subnet-mask 0))
  ([subnet-mask i]
   (let [byte-index (- (count subnet-mask) (quot i 8))
         bit-index  (mod i 8)]
     (when (pos? byte-index)
       (let [bit (bit-test (nth subnet-mask byte-index) bit-index)]
         )))))
