#lang racket
(define (read-token)
  (let ((first-char (read-char)))
      (cond ;; if first-char is a space or line break, just skip it
            ;; and loop to try again by calling self recursively
            ((char-whitespace? first-char)
             (read-token))
            ;; else if it's a left paren char, return the special
            ;; object that we use to represent left parenthesis tokens.
            ((eq? first-char #\( )
             left-paren-token)
            ;; likewise for right parens
            ((eq? first-char #\) )
             right-paren-token
            ;; else if it's a letter, we assume it's the first char
            ;; of a symbol and call read-identifier to read the rest of
            ;; of the chars in the identifier and return a symbol object
            ((char-alphabetic? first-char)
             (read-identifier first-char))
            ;; else if it's a digit, we assume it's the first digit
            ;; of a number and call read-number to read the rest of
            ;; the number and return a number object
            ((char-numeric? first-char)
             (read-number first-char))
            ;; else it's something this little reader can't handle,
            ;; so signal an error
            (else
             (error "illegal lexical syntax")))))