; Giuliana Conte gdc24
; EECS 345
; Programming exercise 2


(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))


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

; merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order.
; > (merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10))
; (0 1 2 3 4 5 6 6 7 8 9 9 10)

(define merge
  (lambda (lis1 lis2)
    (merge-cps lis1 lis2 (lambda (v) v))))

(define merge-cps
  (lambda (lis1 lis2 return)
    (cond
      ((null? lis1) (return lis2))
      ((null? lis2) (return lis1))
      ((<= (car lis1) (car lis2)) (merge-cps (cdr lis1) lis2 (lambda (v) (return (cons (car lis1) v)))))
      (else (merge-cps lis1 (cdr lis2) (lambda (v) (return (cons (car lis2) v))))))))

