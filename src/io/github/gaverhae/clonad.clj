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
  [prefix bs]
  (cond (empty? bs) []
        (and (>= (count bs) 3) (= :<< (second bs)))
        (let [[binding-form _ expr & tl] bs]
          [(keyword prefix "bind") expr `(fn [~binding-form] ~(build-monadic-value prefix tl))])
        (= 1 (count bs))
        (first bs)
        :else
        [(keyword prefix "bind") (first bs) `(fn [~(gensym)] ~(build-monadic-value prefix (rest bs)))]))

(defmacro monad
  "Builds a monadic value. Takes a collection of bindings and monadic values;
   a binding is a triplet of the form

       symbol :<< monadic-value

   Example:

       (monad
         a :<< [:pure 3]
         (side-effecting-monadic-value a)
         b :<< (monadic-fun a)
         (other-monadic-fun a b))

   If the very first argument is an unqualified keyword, it will be used as
   the namespace segment for generated bind and pure values."
  [a & bindings-or-statements]
  (cond (and (keyword? a)
             (nil? (namespace a))) (build-monadic-value (name a) bindings-or-statements)
        :else (build-monadic-value nil (cons a bindings-or-statements))))

(defn return
  ([v] [:pure v])
  ([kw v] [(keyword (name kw) "pure") v]))

(defn m-seq
  "[m v] -> m [v]"
  ([mvs] (m-seq (keyword nil "") mvs))
  ([prefix mvs]
   (if (empty? mvs)
     (return prefix ())
     (monad prefix
       v :<< (first mvs)
       r :<< (m-seq prefix (rest mvs))
       (return prefix (cons v r))))))
