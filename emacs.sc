



(load-shared-object "emacs.so")

(define raw
  (foreign-procedure "textmode_raw_mode" () int))

(define get-row-size
    (foreign-procedure "get_row" () int))

(define get-col-size
    (foreign-procedure "get_col" () int))

(define *text* (cons (cons #\x00 '()) '()))
(define *row-size* (get-row-size))
(define *col-size* (get-col-size))



(raw)

(define control
  (lambda ()
    (display #\esc)
    (display #\[)))

(define clean-screem
  (lambda ()
    (control)
    (display #\2)
    (display #\J)))


(define init-mouse
  (lambda ()
    (control)
    (display #\0)
    (display #\;)
    (display #\0)
    (display #\H)))

(clean-screem)
(init-mouse)

(define get-col
  (lambda ()
    (control)
    (display #\6)
    (display #\n)
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




(define move-up
  (lambda ()
    (control)
    (display #\A)))

(define move-down
  (lambda ()
    (control)
    (display #\B)))

(define move-right
  (lambda ()
    (control)
    (display #\C)))

(define move-left
  (lambda ()
    (control)
    (display #\D)))



(define write-out2
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (caar x))
          (write-out2 (cdr x))))))


(define write-out
  (lambda (x)
    (let l1 ((l x))
            (if (not (null? l))
                (begin
                  (display (caar l))
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
                  (display (caar l))
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
      (define c (cons (cons i l) rest))
          (set-cdr! l c)
          (if (null? rest)   
            (display i)
            (let ((next (car rest)))
                 (set-cdr! next c)
                 (write-out c)))))

(define delete-char
  (lambda (l)
    (if (null? (cdar l))
        (alarm l)
        (let ((c (cdar l))
              (rest (cdr l)))
          (set-cdr! c rest)
          (display #\backspace)
          (if (null? rest)
              (begin 
                (display #\space)
                (display #\backspace))
              (let ((next (car rest)))
                   (set-cdr! next c)
                   (write-out3 rest)))
          (input-loop c )))))


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