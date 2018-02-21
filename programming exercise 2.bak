; Giuliana Conte gdc24
; EECS 345
; Programming exercise 2

; insert takes a number and a list of numbers in order and inserts the number in the proper place.
; > (insert 7 '(1 4 5 6 9 10))
; (1 4 5 6 7 9 10)

(define insert
  (lambda (x lis)
    (cond
      ((null? lis) (cons x '()))
      ((<= x (car lis)) (cons x lis))
      (else (cons (car lis) (insert x (cdr lis)))))))

(define insert-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return (cons x '())))
      ((<= x (car lis)) (cons x lis))
      (else (insert-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))