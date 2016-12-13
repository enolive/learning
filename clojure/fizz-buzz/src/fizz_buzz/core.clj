(ns fizz-buzz.core
  (:gen-class))

(defn is-divisible-by? [number denominator]
  (= (mod number denominator) 0))

(def rules (list
             {:denominator 15 :result "Fizz-Buzz"}
             {:denominator 5 :result "Buzz"}
             {:denominator 3 :result "Fizz"}
             ))

(defn generate [number]
  (def matching-rule
    (first
      (filter #(is-divisible-by? number (get % :denominator)) rules))
    )

  (if (not (nil? matching-rule))
    (get matching-rule :result)
    (str number)
    )
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
