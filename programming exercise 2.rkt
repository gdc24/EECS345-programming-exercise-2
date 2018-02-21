; Giuliana Conte gdc24
; EECS 345
; Programming exercise 2

; insert takes a number and a list of numbers in order and inserts the number in the proper place.
; > (insert 7 '(1 4 5 6 9 10))
; (1 4 5 6 7 9 10)

(define insert
  (lambda (x lis)
    (insert-cps x lis (lambda (v) v))))

(define insert-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return (cons x '())))
      ((<= x (car lis)) (return (cons x lis)))                                     ; to do the comparison, it needs another stack frame
      (else (insert-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))  ; to do the cons, it adds another stack frame

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))