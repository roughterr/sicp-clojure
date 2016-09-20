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

;Exercise 1.4