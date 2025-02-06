(defun z-combinator (f)
  ((lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))
   (lambda (x) (funcall f (lambda (y) (funcall (funcall x x) y))))))

(setq fib
  (z-combinator
    (lambda (f)
      (lambda (n)
        (if (<= n 1)
          n
          (+ (funcall f (- n 1)) (funcall f (- n 2))))))))

(let ((n 20))
  (princ "Fibonacci numbers ")
  (princ n)
  (princ " = ")
  (princ (funcall fib n))
  (terpri))
