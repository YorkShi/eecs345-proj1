(load "simpleParser.scm")

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

(define returntree
  (lambda (filename)
    (parser filename)))