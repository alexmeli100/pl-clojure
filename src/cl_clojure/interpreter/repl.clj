(ns cl-clojure.interpreter.repl
  (:require [cl-clojure.interpreter.interpreter :as prolog :refer :all]))

(defn read-in []
  (let [exp (read)
        pred (first exp)]
    `(~pred '~@(rest exp))))

(defn eval-exp []
  (binding [*ns* (find-ns 'cl-clojure.interpreter.interpreter)]
    (eval (read-in))))

(defn write-out []
  (print (eval-exp)))

(defn prolog []
  (loop []
    (print "pl> ")
    (flush)
    (write-out)
    (recur)))