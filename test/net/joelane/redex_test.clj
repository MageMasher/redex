(ns net.joelane.redex-test
  (:require
    [cognitect.transcriptor :as xr]
    [clojure.test :refer :all]
    [net.joelane.redex :as redex]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest transcriptor
  (testing "All *.repl files with cognitect.transcriptor"
    (doseq [repl-file (xr/repl-files "./repl")]
      (is (nil? (xr/run repl-file))))))

(defn bad-add-one
  [a]
  (let [a' (+ 1 a)
        a'' (- a' 1)]
    a''))