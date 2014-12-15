 (ns small)

 (defn call-twice [f x]
   (do
     (f x)
     (f x)))

 (defn read-and-print [filename]
   (let [text (slurp filename)]
     (println text)
     text))

 (def cube-anonymous (fn [x] (* x x x)))

 (defn concat-reverse [a b]
   (concat (reverse a) (reverse b)))

 (defn hasval? [s x]
   (some? (some #{x} s)))

 (defn print-all-pairs [s1 s2]
   (apply println
          (for [x s1
                y s2
                :when (not= x y)]
            [x y])))

 (defn rep [n x] (repeat n x))
