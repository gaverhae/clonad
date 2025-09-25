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

(defn return
  ([v] (return nil v))
  ([kw v] [(if (nil? kw) :pure (keyword (name kw) "pure")) v]))

(def pure return)

(defn build-m-let
  [kw bindings ret]
  (cond (empty? bindings) ret
        (>= (count bindings) 2) (let [[binding-form expr & bindings] bindings]
                                  [(cond (nil? kw) :bind
                                         (keyword? kw) (keyword (name kw) "bind")
                                         :else `(if (nil? ~kw) :bind (keyword (name ~kw) "bind")))
                                   expr
                                   `(fn [~binding-form] ~(build-m-let kw bindings ret))])
        :else (throw (ex-info "m-let bindings requires even number of forms" {}))))

(defmacro m-let
  ([bindings ret] `(m-let nil ~bindings ~ret))
  ([kw bindings ret] (build-m-let kw bindings ret)))

(defn m-seq
  "[m v] -> m [v]"
  ([mvs] (m-seq nil mvs))
  ([prefix mvs]
   (if (empty? mvs)
     (return prefix ())
     (m-let prefix
       [v (first mvs)
        r (m-seq prefix (rest mvs))]
       (return prefix (cons v r))))))
