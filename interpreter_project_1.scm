; eecs345
; project #1
; Yan Ling Cheung

(load "simpleParser.scm")

; the state in the beginning
(define initialstate '(() ()))

; takes a file name and evaluates the code within the file
(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialstate)))

; evaluates each part of the parsetree recursively
(define evaluate
  (lambda (parsetree state)
    (if (null? parsetree)
        state
        (evaluate (cdr parsetree) (M_state (car parsetree) state)))))
  
; return the state after input statement takes place
(define M_state
  (lambda (statement state)
    (cond
      ((eq? (function statement) 'while) (M_state-while (while_condition statement) (while_body statement) state))
      ((eq? (function statement) '=) (M_state-assign (left-operand statement) (expression statement) state))
      ((eq? (function statement) 'if) (if (eq? (length statement) 3)
                                          (M_state-if (if_condition statement) (then statement) state)
                                          (M_state-ifelse (if_condition statement) (then statement) (else statement) state)))
      ((eq? (function statement) 'return) (M_state-return (rexpression statement) state))
      ((eq? (function statement) 'var) (M_state-declare statement state))
      (else (M_val-statement statement state)))))

; returns the state after the while loop is over
(define M_state-while
  (lambda (condition body state)
    (if (not (M_boolean condition state))
        state
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
        (M_state stmt state)
        (M_state elsestmt state))))

; evaluate the state of an assignmnet statement
(define M_state-assign
  (lambda (variable stmt state)
    (if (declared? variable (variablesin state))
        (insert variable (M_val-statement stmt state) (remove variable state))
        (error 'error "undeclared variable"))))

; declared a new variable
(define M_state-declare
  (lambda (stmt state)
    (cond
      ((declared? (newvar stmt) (variablesin state)) (error 'error "variable already existed"))
      ((null? (cddr stmt)) (list (cons (newvar stmt) (variablesin state)) (cons '() (valuesin state))))
      (else (list (cons (newvar stmt) (variablesin state)) (cons (M_val-statement (newval stmt) state) (valuesin state)))))))
        
; check if the variable is declared
(define declared?
  (lambda (n vars)
    (cond
      ((null? vars) #f)
      ((eq? n (car vars)) #t)
      (else (declared? n (cdr vars))))))

; return final value
(define M_state-return
  (lambda (stmt state)
    (cond
      ((eq? (M_val-statement stmt state) #t) 'true)
      ((eq? (M_val-statement stmt state) #f) 'false)
      (else (M_val-statement stmt state)))))

; remove the value that is assigned to the variable
(define state_remove
  (lambda (var state)
    (if (eq? var (firstvar state))
        (list (car state) (cons '() (restofval state)))
        (list (cons (firstvar state) (variablesin (state_remove var (restof state)))) 
              (cons (firstval state) (valuesin (state_remove var (restof state))))))))

; insert a new value to a variable
(define insert
  (lambda (var val state)
    (cond
      ((not (declared? var (variablesin state))) (error 'error "undeclared variable"))
      ((eq? var (firstvar state)) (cons (variablesin state) (cons (cons val (restofval state)) '())))
      (else (list (cons (firstvar state) (variablesin (insert var val (restof state))))
                  (cons (firstval state) (valuesin (insert var val (restof state)))))))))

; return the value of the input statement
(define M_val-statement
  (lambda (stmt state)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) #t)
      ((eq? 'false stmt) #f)
      ((not (list? stmt)) (if (null? (lookup stmt state))
                              (error 'error "variable use before assigning")
                              (lookup stmt state)))
      ((eq? '+ (operator stmt)) (+ (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? '- (operator stmt)) (if (null? (cddr stmt))
                                    (- 0 (M_val-statement (left-operand stmt) state))
                                    (- (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state))))
      ((eq? '* (operator stmt)) (* (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? '/ (operator stmt)) (quotient (M_val-statement (left-operand stmt) state)
                                           (M_val-statement (right-operand stmt) state)))
      ((eq? '% (operator stmt)) (remainder (M_val-statement (left-operand stmt) state)
                                           (M_val-statement (right-operand stmt) state)))
      ((or (eq? (operator stmt) '&&) (eq? (operator stmt) '||) (eq? (operator stmt) '!)) (M_boolean stmt state))
      ((or (eq? (operator stmt) '>) (eq? (operator stmt) '<)) (M_val-compare stmt state))
      ((or (eq? (operator stmt) '>=) (eq? (operator stmt) '<=)) (M_val-compare stmt state))
      ((or (eq? (operator stmt) '!=) (eq? (operator stmt) '==)) (M_val-compare stmt state))
      (else (error 'error "unknown operation")))))

; compare operations
(define M_val-compare
  (lambda (stmt state)
    (cond
      ((eq? (operator stmt) '>) (> (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? (operator stmt) '<) (< (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? (operator stmt) '>=) (>= (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? (operator stmt) '<=) (<= (M_val-statement (left-operand stmt) state) (M_val-statement (right-operand stmt) state)))
      ((eq? (operator stmt) '!=) (not (eq? (M_val-statement (left-operand stmt) state)
                                            (M_val-statement (right-operand stmt) state))))
      ((eq? (operator stmt) '==) (eq? (M_val-statement (left-operand stmt) state)
                                      (M_val-statement (right-operand stmt) state)))
      (else (error 'error "unknown comparison operator")))))

; return the value assigned to a variable
(define lookup
  (lambda (var state)
    (cond
      ((null? (variablesin state)) (error 'error "undeclared variable"))
      ((eq? var (firstvar state)) (firstval state))
      (else (lookup var (restof state))))))

; return if the condition is true in the current state
(define M_boolean
  (lambda (condition state)
    (cond
      ((or (eq? 'true condition) (eq? #t condition)) #t)
      ((or (eq? 'false condition) (eq? #f condition)) #f)
      ((eq? (operator condition) '||) (or (M_boolean (M_val-statement (left-operand condition) state) state)
                                          (M_boolean (M_val-statement (right-operand condition) state) state)))
      ((eq? (operator condition) '&&) (and (M_boolean (M_val-statement (left-operand condition) state) state)
                                          (M_boolean (M_val-statement (right-operand condition) state) state)))
      ((eq? (operator condition) '!) (not (M_boolean (M_val-statement (left-operand condition) state) state)))
      (else (M_val-statement condition state)))))

; not part of the project, used as a reference to see how the parse tree will come out
; return the parse tree
(define returntree
  (lambda (filename)
    (parser filename)))


;----------------------------------------------------------------------------------
; abstractions

; parts in a state
(define firstvar caar)
(define firstval caadr)
(define variablesin car)
(define valuesin cadr)
(define restofval cdadr)

; get the rest of the state
(define restof
  (lambda (state)
    (cons (cdar state) (cons (cdadr state) '()))))

; parts in a declare statement
(define newvar cadr)
(define newval caddr)

; function
(define function car)

; parameter for while
(define while_condition cadr)
(define while_body caddr)

; parameter for assign
(define expression caddr)

; parameters for if
(define if_condition cadr)
(define then caddr)
(define else cadddr)

; parameters for return
(define rexpression cadr)

; parts of a statement
(define operator car)
(define left-operand cadr)
(define right-operand caddr)
