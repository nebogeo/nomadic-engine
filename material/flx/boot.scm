;; [ Copyright (C) 2011 Dave Griffiths : GPLv2 see LICENCE ]

;(define-macro (every-frame . args)
;  `(begin (set! frame-thunk (lambda () ,@args))))

(define eq? eqv?)

(define (diy-macro s)
  (cond
    ((null? s) s)
    ((list? s) 
     (map
      (lambda (i)
        (if (and (list? i) (not (null? i)))
            (cond
              ((eq? (car i) 'with-state) 
               (do-with-state (diy-macro (cdr i))))
              ((eq? (car i) 'with-primitive) 
               (do-with-primitive (diy-macro (cdr i))))
              ((eq? (car i) 'every-frame)
               (do-every-frame (diy-macro (cdr i))))
              (else (diy-macro i)))
            (diy-macro i)))
      s))
    (else s)))

(define (load-pre-process-run filename)
  (define (read-all fi)
    (let ((o (read fi)))
      (cond
       ((eof-object? o) '())
       (else (cons o (read-all fi))))))
  (let ((fi (open-input-file filename)))
    (let ((code (diy-macro (cons 'begin (read-all fi)))))
      (close-input-port fi)
      (eval code))))

(define (pre-process-run code)
  (let ((code (diy-macro (append '(begin) code))))
    (eval code)))

(define (load-pre-process-get filename)
  (define (read-all fi)
    (let ((o (read fi)))
      (cond
       ((eof-object? o) '())
       (else (cons o (read-all fi))))))
  (let ((fi (open-input-file (string-append "material/" filename))))
    (when (not fi) (display filename)(display " not found")(newline))
    (let ((code (diy-macro (cons 'begin (read-all fi)))))
      (close-input-port fi)
      code)))

;; makc eval in outer scope for defines
(define-macro (load-m filename)
  (let ((code (load-pre-process-get filename)))
    `(eval ,code)))


(define load load-m)

(load "flx/list.scm")
(load "flx/maths.scm")
(load "flx/input.scm")
(load "flx/random.scm")
(load "flx/flx.scm")
