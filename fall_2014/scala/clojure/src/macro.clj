 (ns macro)

 (defmacro my-or
   ([] nil)
   ([x] x)
   ([x & next]
     `(if ~x ~x (my-or ~@next))))

 (defmacro my-let
   [bindings & body]
   (let [params (partition 2 bindings)
         wrap (fn [body [name val]] `((fn [~name] ~body) ~val))]
     (reduce wrap `(do ~@body) (reverse params)) ))
