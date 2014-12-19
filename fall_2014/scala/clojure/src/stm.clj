 (ns stm
   (:import (java.util.concurrent Executors)))

(def accounts (reduce (fn [m i]
                        (assoc m i (ref 1000)))
                      {}
                      (range 5)))

 (defn transfer [from_id to_id amount]
   (dosync
     (alter (accounts from_id) - amount)
     (alter (accounts to_id) + amount)))

 (defn test-transfer [iterations]
   (let [pool (Executors/newFixedThreadPool 4)
         tasks (map (fn [id]
                      (fn []
                        (dotimes [n iterations]
                          (transfer id
                                    (+ 1 id)
                                    (+ id 5)))))
                    (range 4))]
     (doseq [future (.invokeAll pool tasks)]
       (.get future))
     (.shutdown pool)))

 (do
   (test-transfer 100)
   (apply println (map deref (reverse (vals accounts)))))