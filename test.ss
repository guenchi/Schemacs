


(raw-off)
(clean-screem)
(init-mouse)

(define restart
  (lambda ()
    (raw-on)
    (init)
    (welcome)
    (message)
    (move-to (row-size) 0)        
    (start)))

(define write-act
  (lambda (x)
    (if (not (null? x))
        (begin
          (display (car (loc-info x)))
          (display " , ")
          (display (cdr (loc-info x)))
          (if (car (acts-info x)) (display-with 'white " insert ") (display-with 'red " delete "))
          (write (cdr (acts-info x)))
          (newline)
          (write-act (cdr x))))))


(display "text actual:")
(newline)
(write-out *text*)
(newline)
(display "action chain:")
(newline)
(write-act (cdr *acts*))
(newline)
(display "(exit) to exit, (restart) to restart.")
    