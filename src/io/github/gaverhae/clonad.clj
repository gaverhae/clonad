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

(defn m-seq
  "[m v] -> m [v]"
  [mvs]
  (if (empty? mvs)
    [:pure ()]
    (mdo [v (first mvs)
          r (sequenceM (rest mvs))
          _ [:pure (cons v r)]])))

(defn m-map
  "(a -> m b) -> [a] -> m [b]"
  [f args]
  (m-seq (map f args)))
