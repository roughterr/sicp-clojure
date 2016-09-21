(def hello (fn [] "Hello world"))
(hello)

(defn square [x] (* x x))
;(square 4)

(defn sum-of-squares [x y]
  (+ (square x) (square y)))
;(sum-of-squares 3 4)

(defn abs [x]
  "changes sign of a number"
  (cond (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))
(defn abs-with-if [x]
  "changes sign of a number. uses if function inside"
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
(defn middle-of-three [n1 n2 n3]
  "returns not a smallest but not biggest number of given 3 numbers"
  (def biggest-one (biggest-of-three n1 n2 n3))
  (cond (= biggest-one n1) (biggest-of-two n2 n3)
    (= biggest-one n2) (biggest-of-two n1 n3)
    (= biggest-one n3) (biggest-of-two n1 n2)
    )
  )
(defn sum-of-squares-of-two-larger [n1 n2 n3]
  "procedure that takes three numbers
as arguments and returns the sum of the squares of the two
larger numbers"
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
(defn improve-cuberoot [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(defn cubert-iter [old-guess new-guess x]
  (if (good-enough-improved? old-guess new-guess x)
    new-guess
    (cubert-iter new-guess (improve-cuberoot new-guess x) x)))
(defn cuberoot [x] (cubert-iter 0 1.0 x))
;(cuberoot 8)