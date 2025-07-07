(ns user
  (:require [clj-async-profiler.core :as prof]
            [clj-java-decompiler.core :as decomp]
            [clj-memory-meter.core :as mem]))

(defmacro export
  "Export a var from another namespace as a macro."
  [name var]
  `(do
    (defmacro ~name [& args#] `(~(symbol ~var) ~@args#))
    (alter-meta! (var ~name) merge (dissoc (meta ~var) :macro))
    (var ~name)))

;; Clojure Async Profiler.
;; https://github.com/clojure-goes-fast/clj-async-profiler
(export profile #'prof/profile)
(export profile-ui #'prof/serve-ui)

;; Clojure Java Decompiler.
;; https://github.com/clojure-goes-fast/clj-java-decompiler
(export decompile #'decomp/decompile)
(export disassemble #'decomp/disassemble)

;; Clojure Memory Meter.
;; https://github.com/clojure-goes-fast/clj-memory-meter
(export memory #'mem/measure)

;; time+
;; https://clojure-goes-fast.com/kb/benchmarking/time-plus/
(let [time*
      (fn [^long duration-in-ms f]
        (let [^com.sun.management.ThreadMXBean bean (java.lang.management.ManagementFactory/getThreadMXBean)
              bytes-before (.getCurrentThreadAllocatedBytes bean)
              duration (* duration-in-ms 1000000)
              start (System/nanoTime)
              first-res (f)
              delta (- (System/nanoTime) start)
              deadline (+ start duration)
              tight-iters (max (quot (quot duration delta) 10) 1)]
          (loop [i 1]
            (let [now (System/nanoTime)]
              (if (< now deadline)
                (do (dotimes [_ tight-iters] (f))
                    (recur (+ i tight-iters)))
                (let [i' (double i)
                      bytes-after (.getCurrentThreadAllocatedBytes bean)
                      t (/ (- now start) i')]
                  (println
                   (format "Time per call: %s   Alloc per call: %,.0fb   Iterations: %d"
                           (cond (< t 1e3) (format "%.0f ns" t)
                                 (< t 1e6) (format "%.2f us" (/ t 1e3))
                                 (< t 1e9) (format "%.2f ms" (/ t 1e6))
                                 :else (format "%.2f s" (/ t 1e9)))
                           (/ (- bytes-after bytes-before) i')
                           i))
                  first-res))))))]
  (defmacro time+
    "Like `time`, but runs the supplied body for 2000 ms and prints the average
  time for a single iteration. Custom total time in milliseconds can be provided
  as the first argument. Returns the returned value of the FIRST iteration."
    [?duration-in-ms & body]
    (let [[duration body] (if (integer? ?duration-in-ms)
                            [?duration-in-ms body]
                            [2000 (cons ?duration-in-ms body)])]
      `(~time* ~duration (fn [] ~@body)))))
