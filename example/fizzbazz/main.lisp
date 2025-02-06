(defun fizzbuzz (n)
  (cond
    ((= 0 (mod n 15)) "FizzBuzz")
    ((= 0 (mod n 3)) "Fizz")
    ((= 0 (mod n 5)) "Buzz")
    (t n)))

(defun fizzbuzz-loop (n)
  (if (<= n 100)
    (progn
      (princ (fizzbuzz n))
      (terpri)
      (fizzbuzz-loop (+ n 1)))))

(fizzbuzz-loop 1)
