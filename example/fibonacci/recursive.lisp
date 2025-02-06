(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(let ((n 20))
  (princ "Fibonacci numbers ")
  (princ n)
  (princ " = ")
  (princ (fib n))
  (terpri))
