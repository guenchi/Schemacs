



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
          l)
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
          c))))

(define up
  (lambda (loop txt)
    (move-up)
    (loop (read-char) txt)))

(define down
  (lambda (loop txt)
    (move-down)
    (loop (read-char) txt)))

(define right
  (lambda (loop txt)
    (move-right)
    (loop (read-char) (cdr txt))))

(define left
  (lambda (loop txt)
    (move-left)
    (loop (read-char) (cdar txt))))




(let loop ((c (read-char))
           (txt *text*))
          (case c 
            (#\x20
                   (begin
                     (newline)
                     (write-out2 *text*)
                     (loop (read-char) txt)))
            (#\esc
              (if (equal? #\[ (read-char))
                  (case (read-char)
                    (#\A
                      (up loop txt))
                    (#\B
                      (down loop txt))
                    (#\C
                      (right loop txt))
                    (#\D
                      (left loop txt)))))
            (#\delete
                (loop (read-char) (delete-char txt)))
            (else 
              (begin
                (add-char txt c)
                (loop (read-char) (cdr txt))))))


;      ^[[A
; ^[[D ^[[B ^[[C 