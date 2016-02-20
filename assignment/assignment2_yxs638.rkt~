#lang racket

; 1. If the list consists no element or one, return #true
(define inorder?
  (lambda (lis)
    (cond
      ((null? lis) #t)
      ((null? (cdr lis)) #t)
      ((<= (car lis) (car (cdr lis))) (inorder? (cdr lis)))
      (else #f))))

; 2. Use map to calculate the dotproduct, simple and fast
(define dotproduct
  (lambda (l1 l2)
    (cond
    ((or (null? l1) (null? l2)) 0)
    (else (+ (* (car l1) (car l2)) (dotproduct (cdr l1) (cdr l2)))))))

; 3. Define a nmethod to compute every round of the squareroot and then use suqareroot to recursively calculate the result
(define nmethod
  (lambda (a b)
    (- a (/ (- (* a a) b) (* a 2)))))

(define squareroot
  (lambda (a b c)
    (cond
      ((null? b) a)
      (else (squareroot (nmethod a c) (- b 1) c)))))

; 4. Use three conditions to remove the first orrurance of each elements in L1 from L2
(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((eq? (car l1) (car l2)) (removesubsequence (cdr l1) (cdr l2)))
      (else (cons (car l2) (removesubsequence l1 (cdr l2)))))))

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

; 6. use null? and list? to find the first element in list, nested or not
(define first*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (first* (car lis)))
      (else (car lis)))))

; 7. Use and to deal with the exception when the last element of original list is numnber or list
(define last*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((and (null? (cdr lis)) (list? (car lis))) (last* (car lis)))
      ((and (null? (cdr lis)) (number? (car lis))) (car lis))
      (else (last* (cdr lis))))))

; 8. Use the flatten first, then use the inorder? function to solve the problem
(define numorder*?
 (lambda (list)
   (cond
     ((null? list) #t)
     ((null? (cdr list)) #t)
     (else (inorder? (flatten list))))))

; 9. Use previous function dotproduct and calculate the vectormult
(define vectormult
 (lambda (v m)
   (cond
     ((null? m) '())
     (else (cons (dotproduct v (car m)) (vectormult v (cdr m)))))))

; 10. Use map to calculate each row and each column in order
(define matrixmultiply
 (lambda (m1 m2)
  (map
   (lambda (row)
    (apply map
     (lambda column
      (apply + (map * row column)))
     m2))
   m1)))