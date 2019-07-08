



(load-shared-object "emacs.so")

(define raw
  (foreign-procedure "textmode_raw_mode" () int))

(raw)

(define *text* (cons (cons #\x00 '()) '()))

(define move
  (lambda ()
    (display #\esc)
    (display #\[)))

(define move-up
  (lambda ()
    (move)
    (display #\A)))

(define move-down
  (lambda ()
    (move)
    (display #\B)))

(define move-right
  (lambda ()
    (move)
    (display #\C)))

(define move-left
  (lambda ()
    (move)
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
    (let l2 ((len (- (length x) 1)))
            (if (> len 0)
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

(define up
  (lambda (txt)
    (move-up)
    (input-loop txt )))

(define down
  (lambda (txt)
    (move-down)
    (input-loop txt )))

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
          (newline)
          (write-out2 *text*)
          (input-loop txt))))
      (#\delete
        (delete-char txt))
      (else 
        (add-char txt c)
        (input-loop (cdr txt) )))))

        
(input-loop *text* )

