



(load-shared-object "emacs.so")

(define raw
  (foreign-procedure "textmode_raw_mode" () int))

(define get-row-size
    (foreign-procedure "get_row" () int))

(define get-col-size
    (foreign-procedure "get_col" () int))

(raw)

(define *text* (cons (cons (cons #\x00 0) '()) '()))

(define row-size
  (lambda ()
    (virtual-register 0)))

(define col-size
  (lambda ()
    (virtual-register 1)))

(define row 
    (lambda ()
        (virtual-register 2)))

(define set-row! 
  (lambda (x)
    (set-virtual-register! 2 x))) 


(define col 
    (lambda ()
        (virtual-register 3)))

(define set-col! 
  (lambda (x)
    (set-virtual-register! 3 x))) 


(define row-
  (lambda ()
    (if (> (row) 1)
        (set-row! (- (row) 1)))))

(define row+
  (lambda ()
    (if (< (row) (row-size))
        (set-row! (+ (row) 1)))))




(define col+
  (lambda ()
    (if (< (col) (col-size))
        (set-col! (+ (col) 1))
        (begin
         (set-col! 1)
         (set-row! (+ (row) 1))))))


(define col-
  (lambda ()
    (case (col)
      (1
        (set-col! (col-size))
        (set-row! (- (row) 1)))
      (else
        (set-col! (- (col) 1))))))


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
(set-virtual-register! 0 (get-row-size))
(set-virtual-register! 1 (get-col-size))
(set-row! 1)
(set-col! 1)


(define previous cdar)
(define next cdr)
(define payload caaar)

(define set-payload! 
  (lambda (t p)
    (set-car! (caar t) p)))

(define position cdaar)

(define set-position! 
  (lambda (t p)
    (set-cdr! (caar t) p)))

(define conbine! set-cdr!)

(define retrace! 
  (lambda (rest pre)
    (set-cdr! (car rest) pre)))





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


(define move-to
  (lambda (r c)
    (ioctl r #\; c #\H)))



(define write-out
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (payload x))
          (write-out (next x))))))


(define next-position
  (lambda (txt c)
    (case (payload txt)
      (#\newline
        1)
      (else
        (if (= c (col-size))
            1 
            (+ 1 c))))))



(define update-input
  (lambda (txt)
    (let l ((t txt)
            (c (col)))
      (if (not (null? t))
          (begin
            (display (payload t))
            (set-position! t c)
            (l (next t) (next-position t c)))))
    (move-to (row) (col))))


(define update-delete
  (lambda (txt)
    (let l ((t txt)
            (c (col)))
      (if (null? t)
          (display #\space)
          (begin
            (display (payload t))
            (set-position! t c)
            (l (next t) (next-position t c)))))
    (move-to (row) (col))))


(define alarm
  (lambda (txt)
    (display #\alarm)
    (input-loop txt)))


(define input
  (lambda (txt i)
      (define rest (next txt))
      (define t (cons (cons (cons i (col)) txt) rest))
      (conbine! txt t)
      (case i
        (#\newline 
          (row+)
          (set-col! 1))
        (else 
          (col+)))
      (if (null? rest)   
          (display i)
          (begin 
            (retrace! rest t)
            (display i)
            (update-input rest)))
      (input-loop (next txt))))


(define delete
  (lambda (txt)
    (define pre (previous txt))
    (define rest (next txt))
    (if (null? pre)
        (alarm txt)
        (case (payload txt)
          (#\newline
            (conbine! pre rest)
            (if (null? rest)
                (begin 
                  (move-up)
                  (move-right (cdaar pre)))
                (begin
                  (retrace! rest pre)
                  (clean-line)
                  (move-up)
                  (move-right (cdaar pre))
                  (row-)
                  (set-col! (+ (cdaar pre) 1))
                  (update-delete rest)))
            (input-loop pre))
          (else
            (conbine! pre rest)
            (display #\backspace)
            (display #\space)
            (if (> (col) 1)
                    (display #\backspace))
            (col-)
            (if (not (null? rest))
                (begin
                  (retrace! rest pre)
                  (update-delete rest)))
            (input-loop pre))))))


(define switch-row-up
  (lambda (txt)
    (let loop ((c (col-size))(t txt))
      (if (> c 0)
          (loop (- c 1)(previous t))
          t))))


(define switch-row-down
  (lambda (txt)
    (let loop ((c (col-size))(t txt))
      (if (> c 0)
          (loop (- c 1)(next t))
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
    (let ((rest (next txt)))
      (if (null? rest)
          (alarm txt)
          (begin
            (move-right)
            (col+)
            (input-loop rest))))))

(define left
  (lambda (txt)
    (let ((pre (previous txt)))
      (if (null? pre)
          (alarm txt)
          (case (payload pre)
            (#\newline 
              (move-up)
              (move-right (- (cdaar pre) 2))
              (row-)
              (set-col! (cdaar pre))
              (input-loop (previous pre)))
            (else
              (move-left)
              (col-)
              (input-loop pre)))))))


(load "init.sc")



(define  input-loop
  (lambda (txt)
    (define i (read-char))
    (move-to (- (row-size) 1) 1)
    (set-tbgcolor 'white)
    (display " *Emacs on Chez Scheme* [ ")
    (display (row))
    (display ", ")
    (display (col))
    (display " ]                                              ")
    (set-tbgcolor 'black)
    (move-to (row) (col))
    (case i 
      (#\x01
        (c-a txt))
      (#\x02
        (c-b txt))
      (#\tab
        (input-loop txt))
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
          (esc-esc txt))))
      (#\delete
        (delete txt))
      (else 
        (input txt i)))))

        
(input-loop *text*)




