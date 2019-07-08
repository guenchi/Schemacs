



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
        (begin 
          (display #\alarm)
          (input-loop l (read-char)))
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
          (input-loop c (read-char))))))

(define up
  (lambda (txt)
    (move-up)
    (input-loop txt (read-char))))

(define down
  (lambda (txt)
    (move-down)
    (input-loop txt (read-char))))

(define right
  (lambda (txt)
    (move-right)
    (input-loop (cdr txt) (read-char))))

(define left
  (lambda (txt)
    (move-left)
    (input-loop (cdar txt) (read-char))))




(define  input-loop
  (lambda (txt c)
          (case c 
            (#\x20
                   (begin
                     (newline)
                     (write-out2 *text*)
                     (input-loop txt (read-char))))
            (#\esc
              (if (equal? #\[ (read-char))
                  (case (read-char)
                    (#\A
                      (up txt))
                    (#\B
                      (down txt))
                    (#\C
                      (right txt))
                    (#\D
                      (left txt)))))
            (#\delete
                (delete-char txt))
            (else 
              (begin
                (add-char txt c)
                (input-loop (cdr txt) (read-char)))))))

        
(input-loop *text* (read-char))

