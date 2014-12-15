 (ns factorial)

 (defmulti factorial #(<= % 1))

 (defmethod factorial true [x]
   1)

 (defmethod factorial false [x]
   (* x (factorial (- x 1))))
