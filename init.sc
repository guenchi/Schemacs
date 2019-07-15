
;; (set-txtcolor)
;; (set-tbgcolor)
;; 'black 'red 'green 'yellow 'blue 'purple 'dark-green 'white


(define display1
  (lambda (x)
    (if (not (null? x))
        (begin
          (write (caaar x))
          (display " ")
          (display1 (cdr x))))))


(define display2
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (caaar x))
          (display2 (cdr x))))))


(define display3
  (lambda (x)
    (if (not (null? x))
        (begin
          (write (cdaar x))
          (display3 (cdr x))))))


(define c-a
  (lambda (txt act)
    (newline)
    (write-out *text*)
    (newline)
    (display1 *text*)
    (newline)
    (display2 *acts*)
    (newline)
    (display3 *acts*)
    (newline)
    (input-loop txt act)))

