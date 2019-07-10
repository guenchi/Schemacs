



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
  (lambda (txt r c)
    (newline)
    (write-out *text*)
    (input-loop txt r c)))


(define c-b
  (lambda (txt r c)
    (newline)
    (display-with-char *text*)
    (input-loop txt r c)))


(define esc-esc
  (lambda (txt r c)
    (newline)
    (display-with-loc *text*)
    (input-loop txt r c)))