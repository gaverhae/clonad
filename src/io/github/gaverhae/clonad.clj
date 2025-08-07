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
  (cond (= 1 (count bs))
        (first bs)

        (and (symbol? (first bs)) (= :<< (second bs)) (>= (count bs) 3))
        (let [[sym _ expr & tl] bs]
          [:bind expr `(fn [~sym] ~(build-monadic-value tl))])

        (and (>= (count bs) 1) (not (keyword? (first bs))))
        (let [[expr & tl] bs]
          [:bind expr `(fn [~(gensym)] ~(build-monadic-value tl))])
        :else
        (ex-info "Shouldn't happen." bs)))

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
  (let [bs bindings-or-statements]
    (loop [bs bs]
      (cond (empty? bs) :done
            (and (symbol? (nth bs 0))
                 (= :<< (nth bs 1))
                 (not (keyword? (nth bs 2)))) (recur (drop 3 bs))
            (not (keyword? (first bs))) (recur (rest bs))
            :else (throw (ex-info "Binding form looks incorrect; each form should be either a single monadic value or a binding: a symbol followed by :<< followed by a monadic value." {}))))
    (build-monadic-value bs)))

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
