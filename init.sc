
;; (set-txtcolor)
;; (set-tbgcolor)
;; 'black 'red 'green 'yellow 'blue 'purple 'dark-green 'white


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
          (display (cdaar x))
          (display3 (cdr x))))))


(define c-a
  (lambda (txt act)
    (newline)
    (write-out *text*)
    (newline)
    (display2 *acts*)
    (newline)
    (display3 *acts*)
    (input-loop txt act)))

