
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
  (lambda (txt)
    (newline)
    (write-out *text*)
    (input-loop txt)))


(define c-b
  (lambda (txt)
    (newline)
    (display-with-char *text*)
    (input-loop txt)))


(define esc-esc
  (lambda (txt)
    (newline)
    (display-with-loc *text*)
    (input-loop txt)))