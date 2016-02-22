#lang racket


; 1. Use map to calculate the dotproduct, simple and fast
(define dotproduct-cps
  (lambda (l1 l2 return)
    (cond
    ((or (null? l1) (null? l2)) (return 1))
    (else (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (return (* v(* (car l1) (car l2))))))))))

; 2. Define a nmethod to compute every round of the squareroot and then use suqareroot to recursively calculate the result
(define squareroot
  (lambda (value it)
    (if (zero? it)
        value
        (- (squareroot value (- it 1)) (/ (- (* (squareroot value ( - it 1))
        (squareroot value (- it 1))) value) (* 2 (squareroot value (- it 1 ))))))))

; 3. Use three conditions to remove the first orrurance of each elements in L1 from L2
(define removesubsequence
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      ((eq? (car l1) (car l2)) (removesubsequence (cdr l1) (cdr l2)))
      (else (cons (car l2) (removesubsequence l1 (cdr l2)))))))

; 4. Replaceall in cps style

(define removeall*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (removeall* a (car l)) (removeall* a (cdr l))))
      ((eq? (car l) a) (removeall* a (cdr l)))
      (else (cons (car l) (removeall* a (cdr l)))))))

; 5. In reverse* function, use append to keep the format of list
(define reverse*
  (lambda (lst) 
   (cond ((null? lst) '() ) 
         ((pair? (car lst)) 
          (append 
           (reverse* (cdr lst)) 
           (list (reverse* (car lst))))) 
         (else 
          (append 
           (reverse* (cdr lst)) 
           (list (car lst)))))))

; 6. Use previous function dotproduct and calculate the vectormult
(define vectormult
 (lambda (v m)
   (cond
     ((null? m) '())
     (else (cons (dotproduct-cps v (car m)) (vectormult v (cdr m)))))))

; 7. matrixmultiply in cps

; 8. removesubsequence*



; 9. Write the following function without external helper functions or additional parameters. You do not need to use continuation passing style, but you may use continuations or call-with-current-continuation to assist you. The function suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom.
    (define suffix
      (lambda (a l)
        (letrec ((suffix-cps
          (lambda (a l k)
            (cond
              ((null? l) (k '()))
              ((eq? a (car l)) (suffix-cps a (cdr l) (lambda (v) v)))
              (else (suffix-cps a (cdr l) (lambda (v) (k (cons (car l) v)))))
              ))))
          (suffix-cps a l (lambda (v) v))
          )))