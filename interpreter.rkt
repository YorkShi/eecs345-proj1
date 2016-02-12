; still missing: M_value, M_boolean, Insert, Remove, M_state-return

(load "simpleParser.scm")

; the state in the beginning
(define initialstate '(() ()))

(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialstate)))

(define evaluate
  (lambda (parsetree state)
    (if (null? parsetree)
        state
        (evaluate (cdr parsetree) (M_state (car parsetree) state)))))
  
(define M_state
  (lambda (statement state)
    (cond
      ((eq? (function statement) 'while) (M_state-while (while_condition statement) (while_body statement) state))
      ((eq? (function statement) '=) (M_state-assign (variable statement) (expression statement) state))
      ((eq? (function statement) 'if) (if (eq? (length statement) 3)
                                          (M_state-if (if_condition statment) (then statement) state)
                                          (M_state-ifelse (if_condition statement) (then statement) (else statement) state)))
      ((eq? (function statement) 'return) (M_state-return (rexpression statement) state))
      ((eq? (function statement) 'var) (if (eq? (length statement) 3)
                                           (M_state-var (variable statement) (vexpression statement) statement)
                                           (M_state-declare (variable statement) statement)))
      (else (error 'unkown "unknow function")))))

; returns the state after the while loop is over
(define M_state-while
  (lambda (condition body state)
    (if (not (M_boolean condition state))
        (M_state condition state)
        (M_state-while condition body (M_state body state)))))

; evaluate the state of an if-then statment
(define M_state-if
  (lambda (condition stmt state)
    (if (M_boolean condition state)
        (M_state stmt state)
        state)))

; evaluate the state of an if-then-else statment
(define M_state-ifelse
  (lambda (condition stmt elsestmt state)
    (if (M_boolean condition state)
        (M_statement stmt state)
        (M_state elsestmt state))))

; evaluate the state of an assignmnet statement
(define M_state-assign
  (lambda (variable expression state)
    (if (declared? variable (variables state))
        (assign variable expression state)
        (error 'undeclared "cannot use an undeclared variable"))))

; check if the variable is declared
(define declared?
  (lambda (n variables)
    (cond
      ((null? variables) #f)
      ((eq? n (car variables) #t)
      (else (declared? n (cdr variables))))))
      
; remove the value that is assigned to the variable
(define remove
  (lambda (var state)
    (if (eq? var (caar state))
        (cons (car state) (cons (cons 'no-value (cdadr state)) '()))
        (remove var (restof state)))))

; get the rest of the state
(define restof
  (lambda (state)
    (cons (cdar state) (cons (cdadr state) '()))))

; not part of the project, used as a reference to see how the parse tree will come out
; return the parse tree
(define returntree
  (lambda (filename)
    (parser filename)))


;----------------------------------------------------------------------------------
; abstractions

; function
(define function car)

; variables in the state
(define variables car)

; parameter for while
(define while_condition cadr)
(define while_body caddr)

; parameter for assign
(define variable cadr)
(define expression caddr)

; parameters for if
(define if_condition cadr)
(define then caddr)
(define else cadddr)

; parameters for return
(define rexpression cadr)

; parameters for var
(define vexpression caddr)
(define variable cadr)
