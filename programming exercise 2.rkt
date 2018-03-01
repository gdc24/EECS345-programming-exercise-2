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
; done!

(define insert
  (lambda (x lis)
    (insert-cps x lis (lambda (v) v))))

(define insert-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return (cons x '())))
      ((<= x (car lis)) (return (cons x lis)))
      (else (insert-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order.
; > (merge '(3 5 6 7 9) '(0 1 2 4 6 8 9 10))
; (0 1 2 3 4 5 6 6 7 8 9 9 10)
; done!

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
; done!

(define removedups
  (lambda (lis)
    (removedups-cps lis (lambda (v) v))))

(define removedups-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((null? (cdr lis)) (return (cons (car lis) '())))
      ((eq? (car lis) (car (cdr lis))) (removedups-cps (cons (car lis) (cddr lis)) (lambda (v) (return v))))
      (else (removedups-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; numparens takes a list and returns the number of pairs of parentheses
; > (numparens '(1 2 3))
; 1
; > (numparens '(1 () (()) (2 3 (4)))
; 6
; done!

(define numparens
  (lambda (lis)
    (numparens-cps lis (lambda (v) v))))

(define numparens-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 1))
      ((list? (car lis)) (numparens-cps (car lis) (lambda (v) (return (+ (numparens-cps (cdr lis) (lambda (v) v)) v)))))  ; uses two stack frames when calling (car lis) and 3 for +, numparens-cps, and (cdr lis)
      (else (numparens-cps (cdr lis) (lambda (v) (return v)))))))


; dup* takes a list and duplicates all contents, including any sublists
; > (dup* '(1 2 (3 4) 5))
; (1 1 2 2 (3 3 4 4) (3 3 4 4) 5 5)
; not done; doesn't duplicate the sublist

(define dup*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((pair? (car lis)) (cons (dup* (car lis)) (cons (dup* (car lis)) (dup* (cdr lis)))))
      (else (cons (car lis) (cons (car lis) (dup* (cdr lis))))))))

(define dup*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((pair? (car lis)) (dup*-cps (car lis) (lambda (v) (return (cons v (dup*-cps (cdr lis) (lambda (v) (cons (dup*-cps (car lis) (lambda (v) v)) v))))))))
      (else (dup*-cps (cdr lis) (lambda (v) (return (cons (car lis) (cons (car lis) v)))))))))

; removedups* takes a list, that can contain sublists, and removes any atom that is the repeat of the atom that immediately precedes it in the same sublist.
; > (removedups* '(a a (b b b (d d) b ((d) d)) f (f f g)))
; (a (b (d) b ((d) d)) f (f g))
; done but not tested
(define removedups*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removedups* (car lis)) (removedups* (cdr lis))))
      ((null? (cdr lis)) (cons (car lis) '()))
      ((eq? (car lis) (car (cdr lis))) (removedups* (cons (car lis) (cdr (cdr lis)))))
      (else (cons (car lis) (removedups* (cdr lis)))))))

(define removedups*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (car lis)) (removedups*-cps (cdr lis) (lambda (v) (return (cons (removedups*-cps (car lis) (lambda (v) v)) v)))))
      ((null? (cdr lis)) (return (cons (car lis) '())))
      ((eq? (car lis) (car (cdr lis))) (removedups*-cps (cons (car lis) (cddr lis)) (lambda (v) (return v)))) 
      (else (removedups*-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; mergesort takes a list of numbers and returns a sorted version. If you recall the merge sort algorithm,
; you use the CPS version of split from lecture to divide the input list into two lists,
; you recursively call mergesort on each sublist,
; and then you call merge on the two lists returned by the recursive calls to mergesort.
; (mergesort '()) ==> '()
; (mergesort '(8 1 3 9 6 5 7 2 4 10)) ==> '(1 2 3 4 5 6 7 8 9 10)
; done but not done

(define mymergesort
  (lambda (lis)
    (mergesort-cps lis (lambda (v) v))))

(define mergesort-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((null? (cdr lis)) (return lis))
      (else (mergesort-cps (cadr (split-cps lis (lambda (v1 v2) (list v1 v2)))) (lambda (v) (return (merge-cps (mergesort-cps (car (split-cps lis (lambda (v1 v2) (list v1 v2)))) (lambda (v) v)) v (lambda (v) v)))))))))


; the cps version of split
(define split-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((null? (cdr lis)) (return lis '()))
      (else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))



; replaceatoms takes two lists. The first list can contain sublists, but the second list is a single list of atoms.
; The output should be the first list, but each atom of the first list, from left to right, is replaced by the corresponding
; atom of the second list, until the second list runs out of atoms.
; > (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u t s r q p o n m l k j))
; ((z ((y) x w) ((((v) u t) (s r)) q (p o))) n m (l k))
; > (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u))
; ((z ((y) x w) ((((v) u g) (h i)) j (k l))) m n (o p))
; not done

(define replaceatoms
  (lambda (lis1 lis2)
    ((null? lis1) lis1)
    ()))



