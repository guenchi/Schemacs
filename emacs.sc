



(load-shared-object "emacs.so")

(define raw-on
  (foreign-procedure "raw_on" () int))

(define raw-off
  (foreign-procedure "raw_off" () int))

(define get-row-size
    (foreign-procedure "get_row" () int))

(define get-col-size
    (foreign-procedure "get_col" () int))


(define *text* (cons (cons (cons #\x00 0) '()) '()))
(define *acts* (cons (cons (cons (cons '() '()) (cons '() '())) '()) '()))

(define row-size
  (lambda ()
    (virtual-register 0)))

(define set-row-size! 
  (lambda (x)
    (set-virtual-register! 0 x))) 

(define col-size
  (lambda ()
    (virtual-register 1)))


(define set-col-size! 
  (lambda (x)
    (set-virtual-register! 1 x)))

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


(define col-cache
    (lambda ()
        (virtual-register 4)))


(define set-col-cache!
    (lambda ()
        (set-virtual-register! 4 (col))))


(define lines
  (lambda ()
    (virtual-register 5)))


(define set-lines!
  (lambda (x)
    (set-virtual-register! 5 x))) 


(define line
  (lambda ()
    (virtual-register 6)))


(define set-line!
  (lambda (x)
    (set-virtual-register! 6 x))) 


(define footer
  (lambda ()
    (virtual-register 7)))

(define set-footer!
  (lambda (x)
    (set-virtual-register! 7 x)))



(define row-
  (lambda ()
    (if (> (row) 1)
        (set-row! (- (row) 1)))))

(define row+
  (lambda ()
    (if (< (row) (row-size))
        (set-row! (+ (row) 1)))))


(define line-
  (lambda ()
    (set-line! (- (line) 1))))


(define line+
  (lambda ()
    (set-line! (+ (line) 1))))


(define lines-
  (lambda ()
    (set-line! (- (line) 1))
    (set-lines! (- (lines) 1))))


(define lines+
  (lambda ()
    (set-line! (+ (line) 1))
    (set-lines! (+ (lines) 1))))


(define col+
  (lambda ()
    (if (< (col) (col-size))
        (set-col! (+ (col) 1))
        (begin
          (lines+)
          (set-col! 1)
          (set-row! (+ (row) 1))))))


(define col-
  (lambda ()
    (case (col)
      (1
        (lines-)
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


(define auto-info
  (lambda ()
    (move-to (footer) 1)
    (set-tbgcolor 'white)
    (display " *Emacs on Chez Scheme* [ ")
    (display (row))
    (display ", ")
    (display (col))
    (display " ] L")
    (display (line))
    (display " / ")
    (display (lines))
    (clean-line)
    (set-tbgcolor 'black)))


(define message
  (case-lambda
    (()
      (auto-info)
      (move-to (row-size) 1)
      (clean-line)
      (move-to (row) (col)))
    ((str)
      (auto-info)
      (move-to (row-size) 1)
      (display str)
      (clean-line)
      (move-to (row) (col)))))
  

        
(define clean-screem
  (lambda ()
    (ioctl #\2 #\J)))

(define clean-line
  (lambda ()
    (ioctl #\K)))

(define init-mouse
  (lambda ()
    (ioctl #\0 #\; #\0 #\H)))



(define quit
  (lambda ()
    (raw-off)
    (clean-screem)
    (init-mouse)
    (exit)))

(define start
  (lambda ()
    (clean-screem)
    (init-mouse)
    (set! *text* (cons (cons (cons #\x00 0) '()) '()))
    (set! *acts* (cons (cons (cons (cons '() '()) (cons '() '())) '()) '()))
    (set-row-size! (get-row-size))
    (set-col-size! (get-col-size))
    (set-row! 1)
    (set-col! 1)
    (set-line! 1)
    (set-lines! 1)
    (set-footer! (- (row-size) 1))))


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
  


(define welcome
  (lambda ()
    (printf 
"Welcome to Emacs on Chez Scheme



Author:  github.com/guenchi
License: MIT


This implementation of Emacs is written by pure Scheme, and also use Scheme instead of elisp as extended language.


To start: C-x C-f
To quit:  C-x C-c")))



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


(define action
  (lambda (act a c)
    (define i (cons (cons (cons (cons (line) (col)) (cons a c)) act) '()))
      (set-cdr! act i)))


(define loc-info caaar)
(define acts-info cdaar)


(define line-info
  (lambda (act)
    (car (loc-info act))))


(define col-info
  (lambda (act)
    (cdr (loc-info act))))


(define act-info
  (lambda (act)
    (car (acts-info act))))


(define char-info
  (lambda (act)
    (cdr (acts-info act))))



(define del
  (lambda (txt act ch)
    (define pre (previous txt))
    (define rest (next txt))
        (case ch
          (#\newline
            (conbine! pre rest)
            (move-to (row) (col))
            (lines-)
            (if (null? rest)
                (begin 
                  (row-)
                  (set-col! (+ (position pre) 1)))
                (begin
                  (retrace! rest pre)
                  (clean-line)
                  (row-)
                  (set-col! (+ (position pre) 1))
                  (update-delete rest))))
          (else
            (conbine! pre rest)
            (move-to (row) (+ (col) 1))
            (display #\backspace)
            (display #\space)
            (display #\backspace)
            (if (not (null? rest))
                (begin
                  (retrace! rest pre)
                  (update-delete rest)))))
        (input-loop pre (previous act))))



(define undo
  (lambda (txt act)
    (define l (line-info act))
    (define c (col-info act))
    (define up
      (lambda (t)
        (if (or (equal? (payload t) #\newline) 
                (= (position t) 80))
            (begin 
              (row-)
              (line-)))))
    (define down
      (lambda (t)
        (if (or (equal? (payload t) #\newline) 
                (= (position t) 80))
            (begin
              (row+)
              (line+)))))
    (let loop ((t txt))
      (cond 
        ((< l (line))
           (up t)
           (loop (previous t)))
        ((> l (line))
           (down t)
           (loop (next t)))
        ((< c (position t))
           (loop (previous t)))
        ((> c (position t))
           (loop (next t)))
        (else
          (set-col! c)
          (if (act-info act)
              (del t act (char-info act))
             ; (insert t act (char-info act))
             ))))))


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
            (case (payload t)
              (#\newline 
                (clean-line)))
            (display (payload t))
            (set-position! t c)
            (l (next t) (next-position t c)))))
    (move-to (row) (col))))


(define update-delete
  (lambda (txt)
    (let l ((t txt)
            (c (col)))
      (if (or (null? t) 
              (equal? (payload t) 
                      #\newline))
          (display #\space)
          (begin
            (display (payload t))
            (set-position! t c)
            (l (next t) (next-position t c)))))
    (move-to (row) (col))))


(define alarm
  (lambda (txt act)
    (display #\alarm)
    (message "Operating fail~") 
    (input-loop txt act)))


(define input
  (lambda (txt act i)
    (define rest (next txt))
    (define t (cons (cons (cons i (col)) txt) rest))
    (action act i (col))
    (conbine! txt t)
    (case i
      (#\newline
        (row+)
        (lines+)
        (set-col! 1)
        (clean-line))
      (else 
        (col+)))
    (if (null? rest)   
        (display i)
        (begin 
          (retrace! rest t)
          (display i)
          (update-input rest)))
    (message) 
    (input-loop t (next act))))


(define delete
  (lambda (txt act)
    (define pre (previous txt))
    (define rest (next txt))
    (define p (payload txt))
    (action act p (col))
    (if (null? pre)
        (alarm txt act)
        (case p
          (#\newline
            (conbine! pre rest)
            (lines-)
            (if (null? rest)
                (begin 
                  (move-up)
                  (move-right (position pre))
                  (row-)
                  (set-col! (+ (position pre) 1)))
                (begin
                  (retrace! rest pre)
                  (clean-line)
                  (move-up)
                  (move-right (position pre))
                  (row-)
                  (set-col! (+ (position pre) 1))
                  (update-delete rest)))
            (message) 
            (input-loop pre (next act)))
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
            (message) 
            (input-loop pre (next act)))))))


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
  (lambda (txt act)
    (move-up)
    (input-loop (switch-row-up txt) act)))

(define down
  (lambda (txt act)
    (move-down)
    (input-loop (switch-row-up txt) act)))

(define right
  (lambda (txt)
    (let ((rest (next txt)))
      (if (null? rest)
          (alarm txt)
          (begin
            (move-right)
            (col+)
            (input-loop rest act))))))

(define left
  (lambda (txt act)
    (define pre (previous txt))
    (if (null? pre)
        (alarm txt)
        (case (payload txt)
          (#\newline 
            (move-up)
            (move-right (- (position txt) 2))
            (row-)
            (set-col! (position txt))
            (line-)
            (message "") 
            (input-loop pre act))
          (else
            (move-left)
            (col-)
            (message) 
            (input-loop pre act))))))


(load "init.sc")



(define  input-loop
  (lambda (txt act)
    (define i (read-char))
    (case i 
      (#\x01
        (message "C-a") 
        (c-a txt act))
      (#\x02
        (message "C-b") 
        (c-b txt act))
      (#\x15
        (undo txt act))
      (#\x18
        (message "C-x")  
        (case (read-char)
          (#\x03
            (quit))
          (#\x06
            (start)
            (message "C-x C-f")
            (input-loop *text* *acts*))
          (#\u
            (undo txt act))
          (else
            (message "C-x : command not found")
            (input-loop txt act))))
      (#\tab
        (input-loop txt))
      (#\esc
        (case (read-char)
          (#\[
            (case (read-char)
              (#\A
                (up txt act))
              (#\B
                (down txt act))
              (#\C
                (right txt act))
              (#\D
                (left txt act))))
        (#\esc
          (esc-esc txt act))))
      (#\delete
        (delete txt act))
      (else 
        (input txt act i)))))

(let ()
  (raw-on)
  (start)
  (welcome)
  (message)        
  (input-loop *text* *acts*))




