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

; removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it.
; > (removedups '(a a b b b c c a b b))
; (a b c a b)

(define removedups
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (cons (car lis) '()))
      ((eq? (car lis) (car (cdr lis))) (removedups (cons (car lis) (cdr (cdr lis)))))
      (else (cons (car lis) (removedups (cdr lis)))))))

(define removedups-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((null? (cdr lis)) (return (cons (car lis) '())))
      ((eq? (car lis) (car (cdr lis))) (removedups-cps (cons (car lis) (cdr (cdr lis))) (lambda (v) v)))
      (else (removedups-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; numparens takes a list and returns the number of pairs of parentheses
; > (numparens '(1 2 3))
; 1
; > (numparens '(1 () (()) (2 3 (4)))
; 6

(define numparens
  (lambda (lis)
    (cond
      ((null? lis) '1)
      ((list? (car lis)) (+ (numparens (car lis)) (numparens (cdr lis))))
      (else (numparens (cdr lis))))))

(define numparens-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 1))
      ((list? (car lis)) (numparens-cps (car lis) (lambda (v) (return (+ (numparens-cps (cdr lis) (lambda (v) v)) v)))))  ; uses two stack frames when calling (car lis) and 3 for +, numparens-cps, and (cdr lis)
      (else (numparens-cps (cdr lis) (lambda (v) (return v)))))))
      


