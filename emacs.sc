



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


(define previous cdar)
(define next cdr)
(define payload caaar)
(define conbine! set-cdr!)

(define retrace! 
  (lambda (rest pre)
    (set-cdr! (car rest) pre)))

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


(define move-to
  (lambda (r c)
    (ioctl r #\; c #\H)))



(define write-out
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (payload x))
          (write-out (next x))))))





(define update-input
  (lambda (txt r c)
    (let l1 ((l txt)
             (c c))
            (if (not (null? l))
                (begin
                  (display (payload l))
                  (set-cdr! (caar l) c)
                  (l1 (next l) (col+ c (payload l))))))
    (move-to (- r 1) c)))


(define update-delete
  (lambda (txt r c)
    (let l1 ((l txt)
             (c c))
            (if (null? l)
                (display #\space)
                (begin
                  (display (payload l))
                  (set-cdr! (caar l) c)
                  (l1 (next l) (col+ c (payload l))))))
    (move-to (- r 1) c)))


(define alarm
  (lambda (l r c)
    (display #\alarm)
    (input-loop l r c)))


(define input
  (lambda (txt r c i)
      (define rest (next txt))
      (define t (cons (cons (cons i c) txt) rest))
      (conbine! txt t)
      (if (null? rest)   
          (display i)
          (begin 
            (retrace! rest t)
            (display i)
            (update-input rest r (col+ c i))))
      (input-loop (next txt) r (col+ c i))))


(define delete
  (lambda (txt r c)
    (define pre (previous txt))
    (define rest (next txt))
    (if (null? pre)
        (alarm txt r c)
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
                  (update-delete rest r (+ (cdaar pre) 1))))
            (input-loop pre r (+ (cdaar pre) 1)))
          (else
            (conbine! pre rest)
            (display #\backspace)
            (display #\space)
            (display #\backspace)
            (if (not (null? rest))
                (begin
                  (retrace! rest pre)
                  (update-delete rest r (col- c))))
            (input-loop pre r (col- c)))))))


(define switch-row-up
  (lambda (txt)
    (let loop ((c *col-size*)(t txt))
      (if (> c 0)
          (loop (- c 1)(previous t))
          t))))


(define switch-row-down
  (lambda (txt)
    (let loop ((c *col-size*)(t txt))
      (if (> c 0)
          (loop (- c 1)(next t))
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
    (let ((rest (next txt)))
      (if (null? rest)
          (alarm txt r c)
          (begin
            (move-right)
            (input-loop rest r (+ c 1)))))))

(define left
  (lambda (txt r c)
    (let ((pre (previous txt)))
      (if (null? pre)
          (alarm txt r c)
          (case (payload pre)
            (#\newline 
              (move-up)
              (move-right (- (cdaar pre) 2))
              (input-loop (previous pre) r (cdaar pre)))
            (else
              (move-left)
              (input-loop pre r (- c 1))))))))


(load "init.sc")



(define  input-loop
  (lambda (txt r c)
    (define i (read-char))
    (case i 
      (#\x01
        (c-a txt r c))
      (#\x02
        (c-b txt r c))
      (#\tab
        (input-loop txt r c))
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
          (esc-esc txt r c))))
      (#\delete
        (delete txt r c))
      (else 
        (input txt r c i)))))

        
(input-loop *text* 0 1)




