
;; (set-txtcolor)
;; (set-tbgcolor)
;; 'black 'red 'green 'yellow 'blue 'purple 'dark-green 'white


(define display-with-char
  (lambda (x)
    (if (not (null? x))
        (begin
          (write (caaar x))
          (display-with-char (cdr x))))))


(define display-with-loc
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (cdaar x))
          (display-with-loc (cdr x))))))


(define c-a
  (lambda (txt act)
    (newline)
    (write-out *acts*)
    (input-loop txt act)))


(define c-b
  (lambda (txt act)
    (newline)
    (display-with-char *text*)
    (input-loop txt act)))


(define esc-esc
  (lambda (txt act)
    (newline)
    (display-with-loc *text*)
    (input-loop txt act)))