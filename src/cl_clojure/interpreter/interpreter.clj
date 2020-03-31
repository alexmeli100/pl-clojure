(ns cl-clojure.interpreter.interpreter)

(declare unify prove)


(def atom? (complement coll?))

(defn reuse-cons [x y l]
  (if (and (= x (first l)) (= y (rest l)))
    l
    (cons x y)))

(def fail nil)
(def no-bindings {})

(defn variable? [v]
  (and (symbol? v) (Character/isUpperCase (nth (name v) 0))))

(defn get-bindings [var bindings]
  (find bindings var))

(defn loopup [var bindings]
   (bindings var))

(defn add-bindings [var val bindings]
  (assoc bindings var val))

(defn unify-var [var x bindings]
  (cond
    (get-bindings var bindings)
      (unify (loopup var bindings) x bindings)
    (and (variable? x) (get-bindings x bindings))
      (unify var (loopup x bindings) bindings)
    :else (add-bindings var x bindings)))

(defn unify
([x y] (unify x y no-bindings))
([x y bindings]
  (cond
    (= bindings fail) fail
    (= x y) bindings
    (variable? x) (unify-var x y bindings)
    (variable? y) (unify-var y x bindings)
    (and (seq? x) (seq? y)) (unify (rest x) (rest y) (unify (first x) (first y) bindings))
    :else fail)))

(defn subst-bindings [bindings x]
  (cond
    (= bindings fail) fail
    (= bindings no-bindings) x
    (and (variable? x) (get-bindings x bindings))
      (subst-bindings bindings (loopup x bindings))
    (and (some? x) (atom? x)) x
    (empty? x) x
    :else
      (reuse-cons
        (subst-bindings bindings (first x))
        (subst-bindings bindings (rest x))
        x)))

(defn unifier [x y]
  (subst-bindings (unify x y) x))

(defn clause-head [clause] (first clause))
(defn clause-body [clause] (rest clause))

(def database (atom {}))

(defn get-clauses [pred] ((deref database) pred))
(defn predicate [relation] (first relation))

(defn add-clause [clause]
  (let [pred (predicate (clause-head clause))
        cls (concat (get-clauses pred) (list clause))]
    (swap! database assoc pred cls)))

(defn clear-db []
  (reset! database {}))

(defmacro <- [& clause]
  `(add-clause (list ~@clause)))

(defn sublis [m tree]
  (cond
    (atom? tree)
      (let [val (m tree)]
        (if val
          val
          tree))
    (empty? tree) tree
    :else
      (cons
        (sublis m (first tree))
        (sublis m (rest tree)))))

(defn unique-vars [tree found]
  (cond
    (atom? tree)
      (if (variable? tree)
        (conj found tree)
        found)
    (empty? tree) found
    :else
      (unique-vars
        (first tree)
        (unique-vars (rest tree) found))))

(defn variables [exp]
  (unique-vars exp #{}))

(defn rename-vars [exp]
  (sublis
    (reduce #(assoc %1 %2 (gensym (name %2))) {} (variables exp))
    exp))

(defn prove-all [goals bindings]
  (cond
    (= bindings fail) fail
    (or (empty? goals) (nil? goals)) bindings
    :else (prove (first goals) bindings (rest goals))))

(defn prove [goal bindings other-goals]

  (let [clauses (get-clauses (predicate goal))]
    (if (seq? clauses)
      (some
        (fn [clause]
          (let [new-clause (rename-vars clause)]
            (prove-all
              (concat (clause-body new-clause) other-goals)
              (unify goal (clause-head new-clause) bindings))))
        clauses)
      (apply clauses [(rest goal) bindings other-goals]))))

(defn continue []
  (case (read-line)
    ";" true
    "." nil
    (do
      (println "Type ; to see more or . to stop")
      (flush)
      (continue))))

(defn show-pl-vars [vars bindings other-goals]
  (if (empty? vars)
    (println "Yes")
    (doseq [var vars]
      (printf "%s = %s", var, (subst-bindings bindings var)) (flush)))
  (if (continue)
    fail
    (prove-all other-goals bindings)))

(defn top-level [goals]
  (do
    (swap! database assoc 'cl-clojure.interpreter.interpreter/show-pl-vars show-pl-vars)
    (prove-all
      `(~@goals (show-pl-vars ~@(variables goals)))
      no-bindings)
    (println "No.")))

(defmacro ?- [& goals]
  `(top-level (list ~@goals)))