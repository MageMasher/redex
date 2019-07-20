(ns net.joelane.redex
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *read-eval* *data-readers*])
  (:require
    [clojure.main :as main]
    [clojure.core.logic :as logic]
    [clojure.core.logic.unifier :as unifier]
    [clojure.walk :as walk]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]
    [clojure.tools.reader.reader-types :as t]
    [clojure.tools.reader :refer [read read-string *default-data-reader-fn* *read-eval* *data-readers*]]
    [clojure.java.io :as io]
    [clojure.java.io :as io])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io File StringReader)
           (java.nio.file FileSystems)))

(set! *print-length* 10)

(defonce registry-ref (atom {}))

(defn compile-rule-map [rule-map]
  (let [{:keys [from to]} (unifier/prep rule-map)]
    (assoc rule-map :compiled-rules (doall (map
                                             #(vector
                                                (fn [expr] (logic/== expr %1))
                                                (fn [expr] (logic/== expr %2)))
                                                from
                                                (repeatedly (count from) (constantly to)))))))


(defn simplify-one [expr]
  (let [rules (vec (apply concat (vals @registry-ref)))     ;TODO, iterate over map entries instead of vals, then you can get rule category and add it to metadata
        alts (logic/run* [q]
                         (logic/fresh [pat subst]
                                      (logic/membero [pat subst] rules)
                                      (logic/project [pat subst]
                                                     (logic/all (pat expr)
                                                                (subst q)))))]
    (if (empty? alts) expr (first alts))))

(defn simplify [expr]
  (let [iterated (iterate (partial walk/prewalk #(simplify-one %)) expr)
        partitioned (partition 2 1 iterated)
        dropped (drop-while #(apply not= %) partitioned)
        simplified? (ffirst dropped)]
    (when (not= simplified? expr)
      simplified?)))

(defn find-files
  ([]
   (find-files "glob:*.{edn,repl,clj,cljs,cljc}"))
  ([glob]
   (concat (find-files "./src" glob)
           (find-files "./test" glob)))
  ([directory glob]
   (let [grammar-matcher (.getPathMatcher
                           (FileSystems/getDefault)
                           glob)]
     (->> directory
          clojure.java.io/file
          file-seq
          (filter #(.isFile ^File %))
          (filter #(.matches grammar-matcher (.getFileName (.toPath %))))
          (mapv #(.getAbsolutePath %))))))

(defn read-forms [file]
  (let [rdr (-> file
                io/file
                io/reader
                LineNumberingPushbackReader.
                (t/source-logging-push-back-reader 2 file))
        sentinel ::EOF]
    (loop [forms []]
      (let [form (read {:eof sentinel} rdr)]
        (if (= sentinel form)
          forms
          (recur (conj forms form)))))))

(defn compile-rule-maps
  [rule-maps]
  (map (fn [rule-map] (compile-rule-map rule-map)) rule-maps))

(defn update-registry
  [compiled-rules-map]
  (doseq [{:keys [rule compiled-rules]} compiled-rules-map]
    (swap! registry-ref assoc rule compiled-rules)))

(defn read-rules [file] ;; TODO use slurp, read from URI, not file
  (with-open [rdr (-> file
                      io/file
                      io/reader
                      LineNumberingPushbackReader.)]
    (let [sentinel ::EOF]
      (->
        (read {:eof sentinel} rdr)
        compile-rule-maps
        update-registry))))

(defn -main
  "Entry point for the redex-repl"
  [& args]
  (doseq [file (find-files "./resources" "glob:*.edn")]
    (read-rules file))
  (doseq [simplified (keep identity (for [file (find-files)
                               forms (read-forms file)
                               form forms]
                           (let [simpler-form? (simplify form)
                                 loc (meta form)]
                             (when simpler-form?
                               (assoc loc
                                 :form form
                                 :alt simpler-form?)))))]
    (pp/pprint simplified)))


;; Repl stuff
(comment
  (defn simplify>
    [{form ::form}]
    (let [simpleform (-> form simplify)]
      (when simpleform
        (prn)
        (flush)
        (println "Did you know you could simplify the previous form?")
        (flush)
        (pp/write simpleform :dispatch clojure.pprint/code-dispatch)
        (flush))))

  (defn sourcelogging-read
    ([opts ^LineNumberingPushbackReader reader]
     (let [[_ s] (read+string opts reader)
           re-reader (-> (StringReader. s)
                         (LineNumberingPushbackReader.)
                         (t/source-logging-push-back-reader 2))]
       (read opts re-reader))))

  (defn redex-read
    [request-prompt request-exit]
    (or ({:line-start request-prompt :stream-end request-exit}
         (main/skip-whitespace *in*))
        (let [input (sourcelogging-read {:read-cond :allow} *in*)]
          (main/skip-if-eol *in*)
          (tap> {::form     input
                 ::bindings (get-thread-bindings)})
          input)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn -main
    "Entry point for the redex-repl"
    [& args]
    (do
      (map read-rules (find-files "./dev/redex" "glob:*.edn")))
    (add-tap simplify>)
    ;;(add-tap println-tap>)
    (main/repl :read redex-read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
