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
  (cond (= b 1) a (even? b) (fast-multiplication-rec (+ a a) (/ b 2)) :else (+ a (fast-multiplication-rec a (- b 1)))))

;Exercise 1.18
(defn fast-multiplication-iter [a b]
  (defn iter [a b just-add] (cond (= b 1) (+ a just-add)
                              (even? b) (iter (+ a a) (/ b 2) just-add) :else (iter a (- b 1) (+ a just-add))))
  (iter a b 0))

;Exercise 1.19
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
(defn fib [n]
  (fib-iter 1 0 0 1 n))

;Exercise 1.23
(defn next-divisor-guess
  "Returns a next guess for the divisor to a given divisor"
  [x] (if (= x 2) 3 (+ x 2)))

;Exercise 1.21
(defn divides? [a b] (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (next-divisor-guess test-divisor))))
(defn smallest-divisor
  "finds the smallest integral divisor (greater than 1) of a given number n"
  [n] (find-divisor n 2))
;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

;Exercise 1.24
(defn expmod [base exp m]
  (cond (= exp 0) 1
    (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (- exp 1) m)) m)))
(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))
(defn fast-prime? [n times]
  (cond (= times 0) true
    (fermat-test n) (fast-prime? n (- times 1))
    :else false))

;Exercise 1.22
(defn prime? [n]
  (= n (smallest-divisor n)))
(defn runtime
  "Returns the current value of the running Java Virtual Machine's high-resolution time source, in nanoseconds"
  [] (System/nanoTime))
(defn report-prime [elapsed-time]
  (println " *** ")
  (println elapsed-time))
(defn start-prime-test [n start-time]
  (def is-prime (fast-prime? n 4))
  (if is-prime
    (report-prime (- (runtime) start-time)))
  is-prime)
(defn timed-prime-test [n]
  ;(println "Checking whether the following number is a prime number: " n)
  (start-prime-test n (runtime)))
(defn search-for-primes
  "checks the primality of consecutive odd integers in a specified range"
  [larger-than how-many-find]
  (def start-time (runtime))
  (defn iter [larger-than how-many-find]
    (def n "number in question" (+ larger-than 1))
    (if (timed-prime-test n)
      (if (= how-many-find 1) (report-prime (- (runtime) start-time)) (iter n (- how-many-find 1)))
      (iter n how-many-find)))
  (iter larger-than how-many-find))
;(search-for-primes 1000 3)
;(search-for-primes 10000 3)
;(search-for-primes 100000 3)
;(search-for-primes 1000000 3)

;Exercise 1.25
;(defn expmod [base exp m]
;  (rem (fast-expt-iter base exp) m))
;(expmod 1 0 1)
;(expmod1 1 0 1)

;Exercise 1.27
(defn fool-fast-prime? [n]
  (defn iter [i]
    (cond (= i 1) (println n "is a prime number.")
      (not (= (expmod a n n) a)) (println n "is certainly not prime.")
      :else (iter (- i 1))))
  (iter (- n 1)))
;(fool-fast-prime? 17) ; a conventional prime number
;(fool-fast-prime? 18) ; not a prime number
;(fool-fast-prime? 561) ; Carmichael number
;(fool-fast-prime? 1105) ; Carmichael number
;(fool-fast-prime? 1729) ; Carmichael number
;(fool-fast-prime? 2465) ; Carmichael number
;(fool-fast-prime? 2821) ; Carmichael number
;(fool-fast-prime? 6601) ; Carmichael number

;Exercise 1.28
(defn square-with-check
  "checks if a given square is not a nontrivial square root of 1 modulo m. If it is nontrivial, returns -1"
  [a m]
  (def square-result (square a))
  (def modulo (rem square-result m))
  (def is-nontrivial? (and (not (= a (- m 1))) (not (= a 1)) (= modulo 1)))
  (if is-nontrivial? 0 modulo))
(defn miller-rabin-expmod [base exp m]
  (cond (= exp 0) 1
    (even? exp) (square-with-check (miller-rabin-expmod base (/ exp 2) m) m)
    :else (rem (* base (miller-rabin-expmod base (- exp 1) m)) m)))
(defn miller-rabin-test
  "Checks if a given number is prime using Miller-Rabin test"
  [n]
  (def expmod-result (miller-rabin-expmod (+ (rand-int (- n 2)) 2) (- n 1) n))
  (cond (= expmod-result 1)))
(defn fast-prime-miller-rabin? [n times]
  (def expmod-result (miller-rabin-expmod (+ (rand-int (- n 2)) 2) (- n 1) n))
  (cond (= times 0) true (= expmod-result 0) false
    :else (fast-prime-miller-rabin? n (- times 1))))
;(println (fast-prime-miller-rabin? 17 5))
;(println (fast-prime-miller-rabin? 2465 2))