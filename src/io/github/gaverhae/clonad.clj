(ns io.github.gaverhae.clonad)

(defmacro match
  [expr & cases]
  (let [e (gensym)]
    `(let [~e ~expr]
       (case (first ~e)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat)
                           `(let [~(vec (cons '_ (rest pat))) ~e]
                              ~body)])))))))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw
      (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))

(defn build-monadic-value
  [bs]
  (cond (empty? bs) []
        (and (>= (count bs) 3) (= :<< (second bs)))
        (let [[binding-form _ expr & tl] bs]
          [:bind expr `(fn [~binding-form] ~(build-monadic-value tl))])
        (= 1 (count bs))
        (first bs)
        :else
        [:bind (first bs) `(fn [~(gensym)] ~(build-monadic-value (rest bs)))]))

(defmacro monad
  "Builds a monadic value. Takes a collection of bindings and monadic values;
  a binding is a triplet of the form
  symbol :<< monadic-value
  Example:
  (monad
    a :<< [:pure 3]
    (side-effecting-monadic-value a)
    b :<< (monadic-fun a)
    (other-monadic-fun a b))"
  [& bindings-or-statements]
  (build-monadic-value bindings-or-statements))

(defn m-seq
  "[m v] -> m [v]"
  [mvs]
  (if (empty? mvs)
    [:pure ()]
    (mdo [v (first mvs)
          r (m-seq (rest mvs))
          _ [:pure (cons v r)]])))

(defn m-map
  "(a -> m b) -> [a] -> m [b]"
  [f args]
  (m-seq (map f args)))
