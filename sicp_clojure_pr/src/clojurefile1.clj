(def hello (fn [] "Hello world"))
(hello)

(defn square [x] (* x x))
;(square 4)

(defn sum-of-squares [x y]
  (+ (square x) (square y)))
;(sum-of-squares 3 4)

(defn abs "changes sign of a number" [x]
  (cond (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))
(defn abs-with-if "changes sign of a number. uses if function inside" [x]
  (if (< x 0)
    x
    (- x)))
;(abs 1)

;Exercise 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(def a 3)
(def b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
  b a
  )
(cond (= a 4) 6
  (= b 4) (+ 6 7 a) :else 25
  )
(+ 2 (if (> b a) b a))
(* (cond (> a b) a
     (< a b) b
     :else -1)
  (+ a 1))

;Exercise 1.2
(/
  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
  (* 3 (- 6 2) (- 2 7))
  )

;Exercise 1.3
(defn biggest-of-two [n1 n2]
  (if (> n1 n2) n1 n2))
(defn biggest-of-three [n1 n2 n3]
  (biggest-of-two
    (biggest-of-two n1 n2)
    (biggest-of-two n2 n3))
  )
(defn middle-of-three "returns not a smallest but not biggest number of given 3 numbers"
  [n1 n2 n3]
  (def biggest-one (biggest-of-three n1 n2 n3))
  (cond (= biggest-one n1) (biggest-of-two n2 n3)
    (= biggest-one n2) (biggest-of-two n1 n3)
    (= biggest-one n3) (biggest-of-two n1 n2)
    )
  )
(defn sum-of-squares-of-two-larger
  "procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers"
  [n1 n2 n3]
  (+ (square (biggest-of-three n1 n2 n3)) (square (middle-of-three n1 n2 n3)))
  )
(sum-of-squares-of-two-larger 2 3 4)

;Exercise 1.7
(defn average [x y]
  (/ (+ x y) 2))
(defn improve [guess x]
  (average guess (/ x guess)))
(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(defn sqrt [x]
  (sqrt-iter 1.0 x))
(sqrt 9)
;improved
(defn good-enough-improved? [old-guess new-guess x]
  (def difference (abs (- new-guess old-guess)))
  "improved version of the function. If the new guess is not much different from the old one, then stop guessing."
  (< (/ difference new-guess) 0.01))
(defn sqrt-iter-improved [old-guess new-guess x]
  (if (good-enough-improved? old-guess new-guess x)
    new-guess
    (sqrt-iter-improved new-guess (improve new-guess x) x)))
(defn sqrt-improved [x]
  (sqrt-iter-improved 0 1.0 x))
;(sqrt 0.0001)
;(sqrt-improved 0.0001)
;(sqrt 100000000)
;(sqrt-improved 100000000)
;Exercise 1.8
(defn cuberoot [x]
  ;  (.println (System/out) "cuberoot")
  (defn improve-cuberoot [guess]
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (defn cubert-iter [old-guess new-guess]
    (if (good-enough-improved? old-guess new-guess x)
      new-guess
      (cubert-iter new-guess (improve-cuberoot new-guess))))
  (cubert-iter 0 1.0)
  )
(cuberoot 8)

;Exercise 1.10
(defn A [x y]
  (cond (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))))
;(A 1 10)
;(A 2 4)
;(A 3 3)

;Exercise 1.11
(defn f-recursive [n]
  (if (< n 3) n (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3))))))
;(f-recursive 5)
;(f-recursive 7) ;result must be 142

(defn f-iterative [n]
  (defn iter [two-minus one-minus current i]
    (if (= i 4) (+ (* 2 two-minus) (* 4 one-minus) (* 11 current))
      (iter (* 3 current) (+ (* 2 current) two-minus) (+ current one-minus) (- i 1))))
  (if (< n 3) n (iter 0 0 1 n)))
;(f-iterative 7)

;Exercise 1.12
(defn get-elemet-of-pascal-triangle "returns value of an element of the Pascal’s triangle."
  [row column]
  (
    cond (or (= column 0) (= row 0)) 0
    (or (= row 1) (= row column)) 1
    :else (+ (get-elemet-of-pascal-triangle (- row 1) (- column 1)) (get-elemet-of-pascal-triangle row (- column 1)))
    ))
;(get-elemet-of-pascal-triangle 3 5); 6 is expected

;Exercise 1.15
(defn cube [x] (* x x x))
(defn p [x] (- (* 3 x) (* 4 (cube x))))
(defn sine [angle]
  (if (not (> (abs angle) 0.1)) angle
    (p (sine (/ angle 3.0)))))
;(sine 12.15)

;Exercise 1.16
(defn fast-expt-iter [b n]
  (defn iter [b n a] (cond (= n 0) a (even? n) (iter (square b) (/ n 2) a) :else (iter b (- n 1) (* a b))))
  (iter b n 1))
;(fast-expt-iter 2 5)

;Exercise 1.17
(defn fast-multiplication-rec [a b]
  (cond (= b 1) a (even? b) (fast-multiplication (+ a a) (/ b 2)) :else (+ a (fast-multiplication a (- b 1)))))

;Exercise 1.18
(defn fast-multiplication-iter [a b]
  (defn iter [a b just-add] (cond (= b 1) (+ a just-add)
                              (even? b) (iter (+ a a) (/ b 2) just-add) :else (iter a (- b 1) (+ a just-add))))
  (iter a b 0))

;Exercise 1.19
(defn fib [n]
  (fib-iter 1 0 0 1 n))
(defn fib-iter [a b p q count]
  ;  (println "a=" a ", b=" b ", p=" p ", q=" q ", count=" count)
  (cond (= count 0) b
    (even? count)
    (fib-iter a b
      (+ (* p p) (* q q)) ; compute p′
      (+ (* q q) (* 2 (* p q))) ; compute q′
      (/ count 2))
    :else (fib-iter (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p q
            (- count 1))))

;Exercise 1.21
(defn divides? [a b] (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))
(defn smallest-divisor [n] (find-divisor n 2))
;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

;Exercise 1.22
(defn prime? [n]
  (= n (smallest-divisor n)))
(defn current-time "returns the current time" (System/nanoTime))
(defn report-prime [elapsed-time]
  (println " *** ")
  (println elapsed-time))
(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime (- (current-time) start-time))))
(defn timed-prime-test [n]
  (newline)
  (println n)
  (start-prime-test n (current-time)))
(defn search-for-primes
  "checks the primality of consecutive odd integers in a specified range"
  [larger-than how-much-find]
  (def start-time (current-time))
  (defn iter
    [larger-than how-much-find]
    (def n "number in question" (+ larger-than 1))
    (if (prime? n)
      (if (= how-much-find 1) (report-prime (- (current-time) start-time)) (iter n (- how-much-find 1)))
      (iter n how-much-find)))
  (iter larger-than how-much-find))
;(search-for-primes 1000 3)
;(search-for-primes 10000 3)
;(search-for-primes 100000 3)
