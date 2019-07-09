



(load-shared-object "emacs.so")

(define raw
  (foreign-procedure "textmode_raw_mode" () int))

(define get-row-size
    (foreign-procedure "get_row" () int))

(define get-col-size
    (foreign-procedure "get_col" () int))

(define *text* (cons (cons (cons #\x00 0) '()) '()))
(define *row* 0)
(define *col* 0)
(define *row-size* (get-row-size))
(define *col-size* (get-col-size))



(raw)

(define ioctl
  (lambda keys
    (display #\esc)
    (display #\[)
    (let loop ((l keys))
      (if (not (null? l))
          (begin 
            (display (car l))
            (loop (cdr l)))))))
        
(define clean-screem
  (lambda ()
    (ioctl #\2 #\J)))


(define init-mouse
  (lambda ()
    (ioctl #\0 #\; #\0 #\H)))

(clean-screem)
(init-mouse)

(define get-col
  (lambda ()
    (ioctl #\6 #\n)
    (case (read-char)
      (#\033
        (case (read-char)
          (#\[
            (let l1 ()
              (case (read-char)
                (#\;
                  (let l2 ((x (read-char))(y 0))
                    (case x
                      (#\R
                        y)
                      (else 
                        (l2 (read-char) (+ (* y 10) (- (char->integer x) 48)))))))
                (else 
                  (l1))))))))))


(define col+
  (lambda ()
    (if (< *col* *col-size*)
        (set! *col* (+ *col* 1))
        (set! *col* 0))
    *col*))


(define col-
  (lambda ()
    (if (> *col* 0)
        (set! *col* (- *col* 1))
        (set! *col* *col-size*))
    *col*))



(define move-up
  (case-lambda 
    (()(ioctl #\A))
    ((n)(ioctl n #\A))))

(define move-down
  (case-lambda 
    (()(ioctl #\B))
    ((n)(ioctl n #\B))))

(define move-right
  (case-lambda 
    (()(ioctl #\C))
    ((n)(ioctl n #\C))))

(define move-left
  (case-lambda 
    (()(ioctl #\D))
    ((n)(ioctl n #\D))))



(define write-out2
  (lambda (x)
    (if (not (null? x))
        (begin
          (write (caaar x))
          (write-out2 (cdr x))))))


(define write-out
  (lambda (x)
    (let l1 ((l x))
            (if (not (null? l))
                (begin
                  (display (caaar l))
                  (l1 (cdr l)))))
    (let l2 ((len (length x)))
            (if (> len 1)
                (begin
                  (move-left)
                  (l2 (- len 1)))))))


(define write-out3
  (lambda (x)
    (let l1 ((l x))
            (if (null? l)
                (display #\space)
                (begin
                  (display (caaar l))
                  (l1 (cdr l)))))
    (let l2 ((len (+ (length x) 1)))
            (if (> len 0)
                (begin
                  (move-left)
                  (l2 (- len 1)))))))


(define alarm
  (lambda (l)
    (display #\alarm)
    (input-loop l)))


(define add-char
  (lambda (l i)
      (define rest (cdr l))
      (define c (cons (cons (cons i (col+)) l) rest))
          (set-cdr! l c)
          (case i
            (#\newline
              (set! *col* 0)))
          (if (null? rest)   
            (display i)
            (begin 
              (set-cdr! (car rest) c)
              (write-out c)))))


(define delete-char
  (lambda (l)
    (define c (cdar l))
    (define rest (cdr l))
    (if (null? c)
        (alarm l)
        (case (caaar l)
          (#\newline
            (set-cdr! c rest)
            (if (null? rest)
                (begin 
                  (move-up)
                  (move-right (cdaar c)))
                (begin
                  (set-cdr! (car rest) c)
                  (ioctl #\K)
                  (move-up)
                  (move-right (cdaar c))
                  (write-out3 rest)))
            (input-loop c ))
          (else
            (set-cdr! c rest)
            (display #\backspace)
            (if (null? rest)
                (begin 
                  (display #\space)
                  (display #\backspace))
                (begin
                  (set-cdr! (car rest) c)
                  (write-out3 rest)))
            (input-loop c ))))))


(define switch-row-up
  (lambda (txt)
    (let loop ((c *col-size*)(t txt))
      (if (> c 0)
          (loop (- c 1)(cdar t))
          t))))


(define switch-row-down
  (lambda (txt)
    (let loop ((c *col-size*)(t txt))
      (if (> c 0)
          (loop (- c 1)(cdr t))
          t))))

(define up
  (lambda (txt)
    (move-up)
    (input-loop (switch-row-up txt))))

(define down
  (lambda (txt)
    (move-down)
    (input-loop (switch-row-up txt))))

(define right
  (lambda (txt)
    (let ((next (cdr txt)))
      (if (null? next)
          (alarm txt)
          (begin
            (move-right)
            (input-loop next))))))

(define left
  (lambda (txt)
    (let ((before (cdar txt)))
      (if (null? before)
          (alarm txt)
          (begin
            (move-left)
            (input-loop before))))))


(define display-test
  (lambda (txt)
    (newline)
    (write-out2 *text*)
    (input-loop txt)))


(define  input-loop
  (lambda (txt)
    (define c (read-char))
    (case c 
      (#\esc
        (case (read-char)
          (#\[
            (case (read-char)
              (#\A
                (up txt))
              (#\B
                (down txt))
              (#\C
                (right txt))
              (#\D
                (left txt))))
        (#\esc
          (display-test txt))))
      (#\delete
        (delete-char txt))
      (else 
        (add-char txt c)
        (input-loop (cdr txt) )))))

        
(input-loop *text* )








; (define switch-row-up
;   (lambda (txt f)
;     (let loop ((c *col-size*)
;                (c1 (get-col))
;                (t txt))
;       (move-left)
;       (if (> c 0)
;           (if (> (get-col))
;               (case (caar t)
;                 (#\newline
;                   (loop (- c (- c (get-col)))))
;                 (else 
;                   (loop (- c 1)(c1)(cdar t))))
;               (begin
;                 (move-up)
;                 (display #\033)
;                 (display #\[)
;                 (display *col-size*)
;                 (display #\C)
;                 (loop (- c 1)(c1)(cdar t)))
;           t))))