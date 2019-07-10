



(load-shared-object "emacs.so")

(define raw
  (foreign-procedure "textmode_raw_mode" () int))

(define get-row-size
    (foreign-procedure "get_row" () int))

(define get-col-size
    (foreign-procedure "get_col" () int))

(define *text* (cons (cons (cons #\x00 0) '()) '()))
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

(define clean-line
  (lambda ()
    (ioctl #\K)))

(define init-mouse
  (lambda ()
    (ioctl #\0 #\; #\0 #\H)))


(define set-txtcolor
  (lambda (c)
    (ioctl
      (case c
        ('black 30)
        ('red 31)
        ('green 32)
        ('yellow 33)
        ('blue 34)
        ('purple 35)
        ('dark-green 36)
        ('white 37)) #\m)))


(define set-tbgcolor
  (lambda (c)
    (ioctl
      (case c
        ('black 40)
        ('red 41)
        ('green 42)
        ('yellow 43)
        ('blue 44)
        ('purple 45)
        ('dark-green 46)
        ('white 47)) #\m)))
  

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
  (lambda (c i)
    (if (< c *col-size*)
        (case i
          (#\newline 1)
          (else (+ c 1)))
        1)))

(define col-
  (lambda (c)
    (if (> c 1)
        (- c 1)
        *col-size*)))



(define bookmark
  (lambda ()
    (ioctl #\s)))


(define restore
  (lambda ()
    (ioctl #\u)))


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
          (write (cdaar x))
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
  (lambda (l r c)
    (display #\alarm)
    (input-loop l r c)))


(define input
  (lambda (txt r c i)
      (define rest (cdr txt))
      (define t (cons (cons (cons i c) txt) rest))
      (set-cdr! txt t)
      (if (null? rest)   
          (display i)
          (begin 
            (set-cdr! (car rest) t)
            (write-out t)))
      (input-loop (cdr txt) r (col+ c i))))


(define delete
  (lambda (txt r c)
    (define pre (cdar txt))
    (define rest (cdr txt))
    (if (null? pre)
        (alarm txt r c)
        (case (caaar txt)
          (#\newline
            (set-cdr! pre rest)
            (if (null? rest)
                (begin 
                  (move-up)
                  (move-right (cdaar pre)))
                (begin
                  (set-cdr! (car rest) pre)
                  (clean-line)
                  (move-up)
                  (move-right (cdaar pre))
                  (write-out3 rest)))
            (input-loop pre r (- (cdaar txt) 1)))
          (else
            (set-cdr! pre rest)
            (display #\backspace)
            (if (null? rest)
                (begin 
                  (display #\space)
                  (display #\backspace))
                (begin
                  (set-cdr! (car rest) pre)
                  (write-out3 rest)))
            (input-loop pre r (col- c)))))))


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
  (lambda (txt r c)
    (move-up)
    (input-loop (switch-row-up txt))))

(define down
  (lambda (txt r c)
    (move-down)
    (input-loop (switch-row-up txt))))

(define right
  (lambda (txt r c)
    (let ((next (cdr txt)))
      (if (null? next)
          (alarm txt r c)
          (begin
            (move-right)
            (input-loop next r (+ c 1)))))))

(define left
  (lambda (txt r c)
    (let ((before (cdar txt)))
      (if (null? before)
          (alarm txt r c)
          (begin
            (move-left)
            (input-loop before r (- c 1)))))))


(define display-test
  (lambda (txt r c)
    (newline)
    (write-out2 *text*)
    (input-loop txt r c)))


(define  input-loop
  (lambda (txt r c)
    (define i (read-char))
    (case i 
      (#\esc
        (case (read-char)
          (#\[
            (case (read-char)
              (#\A
                (up txt r c))
              (#\B
                (down txt r c))
              (#\C
                (right txt r c))
              (#\D
                (left txt r c))))
        (#\esc
          (display-test txt r c))))
      (#\delete
        (delete txt r c))
      (else 
        (input txt r c i)))))

        
(input-loop *text* 0 1)








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