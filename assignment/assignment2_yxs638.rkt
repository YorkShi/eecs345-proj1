#lang racket

(define (sum n return)
  (if (zero? n)
      return
      (sum (- n 1) (+ return n))))

; 1. Use map to calculate the dotproduct, simple and fast
(define dotproduct-cps
  (lambda (l1 l2 r)
    (if (or (null? l1) (null? l2))
        (r 0)
        (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (r (+ (* (car l1) (car l2)) v)))))))

; 2. Define a nmethod to compute every round of the squareroot and then use suqareroot to recursively calculate the result
(define squareroot
  (lambda (value it r)
    (if (zero? it)
        value
        (- (squareroot value (- it 1)) (/ (- (* (squareroot value ( - it 1))
        (squareroot value (- it 1))) value) (* 2 (squareroot value (- it 1 ))))))))

; I did this wrong last time and this time I figure out how to do it in non-cps style

; 3. Use three conditions to remove the first orrurance of each elements in L1 from L2
(define removesubsequence-cps
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) return))
      (else (removesubsequence-cps l1 (cdr l2) (lambda (v) (return (cons (car l2) v))))))))

; 4. Replaceall in cps style
(define replaceall*-cps
  (lambda (old new l return)
    (cond
      ((null? l) (return '()))
      ((list? (car l)) (replaceall*-cps old new (cdr l) return) (replaceall*-cps old new (car l) return))
      ((eq? old (car l)) (replaceall*-cps old new (cdr l) (lambda(v) (return (cons new (cdr l))))))
      (else (replaceall*-cps old new (cdr l) (lambda(v) (return (cons (car l) (cdr l)))))))))

(define replcae*
  (lambda (old new list return)
    (cond
      ((null? list) (return '()))
      ((pair? (car list)) (replace* old new (car list)
                                    (lambda (v1) (replace* old new (cdr list)
                                                           (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car list) old) (replace* old new (cdr list)
                                      (lambda (v) (return (cons new v)))))
      (else (replace* old new (cdr list) (lambda (v) (return (cons (car list) v))))))))
                                                                                  

; 5. In reverse* function, use append to keep the format of list
; reverse need append

(define append-cps
  (lambda (list1 list2 return)
    (if (null? list1)
        (return list2)
        (append-cps (cdr list1) list2 (lambda (v) (return (cons (car list1) v)))))))

(define reverse*
  (lambda (lst return) 
   (cond ((null? lst) (return '())) 
         ((pair? (car lst)) 
          (append 
           (reverse*- (cdr lst)) 
           (list (reverse*-cps (car lst))))) 
         (else 
          (append 
           (reverse* (cdr lst)) 
           (list (car lst)))))))

(define reverse*-cps
  (lambda (list return)
    (cond
      ((null? list) (return list))
      ((pair? (car list)) (reverse*-cps (car list)
                                        (lambda (v1) (reverse*-cps (cdr list)
                                                                   (lambda (v2) (append-cps v2 (cons v1 '()) return))))))
      (else (reverse*-cps (cdr list) (lambda (v) (append-cps v (cons (car list) '()) return)))))))

; 6. Use previous function dotproduct and calculate the vectormult
(define firstcolumn-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (firstcolumn-cps (cdr m) (lambda (v) (return (cons (caar m) v)))))))

(define restcolumn-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (restcolumn-cps (cdr m) (lambda (v) (return (cons (cdar m) v)))))))

(define vectormult
  (lambda (v m)
    (if (or (null? m) (null? (car m)))
        '()
        (cons (dotproduct v (firstcolumn m)) (vectormult v (restcolumns m))))))

(define vectormult-cps
  (lambda (v m return)
    (if (or (null? m) (null? (car m)))
        (return '())
        (firstcolumn-cps m (lambda (v1)
                             (dotproduct-cps v v1
                                             (lambda (v2)
                                               (restcolumns m
                                                            (lambda (v3)
                                                              (vectormult-cps v v3
                                                                              (lambda (v4)
                                                                                (return cons (v2 v4))))))))))
;(define vectormult-cps
 ;(lambda (v m r)
  ; (cond
   ;  ((null? m) (r '()))
    ; (else (vectormult-cps v (cdr m) (lambda(l) (r (cons (dotproduct-cps v (car m) (lambda(q) q)) l))))))))

; 7. matrixmultiply in cps

(define matrixmultiply
 (lambda (m1 m2)
  (map
   (lambda (row)
    (apply map
     (lambda column
      (apply + (map * row column)))
     m2))
   m1)))

(define matrixmultiply-cps
  (lambda (m1 m2 return)
    (if (null? m1)
        (return '())
        (vectormult-cps (car m1) m2
                        (lambda (v1)
                          (matrixmultiply-cps (cdr m1) m2
                                              (lambda (v2)
                                                (return (cons v1 v2)))))))))
; 8. removesubsequence*
(define removesubsequence-cps
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) return))
      (else (removesubsequence-cps l1 (cdr l2) (lambda (v) (return (cons (car l2) v))))))))

;correct answer
(define removesubsequence*-cps
  (lambda (seq list return)
    (cond
      ((or (null? list) (null? seq)) (return seq list))
      ((pair? (car list)) (removesubsequence*-cps seq (car list)
                                                  (lambda (s1 l1)
                                                    (removesubsequence*-cps s1 (cdr list)
                                                                            (lambda (s2 l2) (return s2 (cons l1 l2)))))))
      ((eq? (car seq) (car list)) (removesubsequence*-cps (cdr seq) (cdr list) return))
      (else (removesubsequence*-cps seq (cdr list)
                                    (lambda (v1 v2)
                                      (return v1 (cons (car list) v2))))))))
                                     
                                  

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