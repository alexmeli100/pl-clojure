(ns cl-clojure.core
  (:require [cl-clojure.interpreter.interpreter :as prolog :refer :all]))

(defn -main
  [& args]
  (do
    (<- '(likes kim robin))
    (<- '(likes robin cats))
    (<- '(likes sandy kim))
    (<- '(likes sandy lee))
    (<- '(likes sandy X) '(likes X cats))
    (<- '(likes kim X) '(likes X lee) '(likes X kim))
    (<- '(likes X X))
    (println (?- '(likes X Y) '(likes Y X)))))
