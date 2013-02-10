
; utils funcs for using lists as sets
(define (set-remove a l)
  (if (null? l)
      '()
      (if (eqv? (car l) a)
          (set-remove a (cdr l))
          (cons (car l) (set-remove a (cdr l))))))		  

(define (set-add a l)
  (if (not (member a l))
      (cons a l)
      l))			  

(define (set-contains a l)
  (if (not (member a l))
      #f
      #t))		  

(define (clear-down)
  (set! keys '()))

(define (foldl op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result) (cdr rest))))
  (iter initial seq))

(define (build-list n fn)
  (define (_ fn n l)
    (display n)(newline)
    (cond ((zero? n) l)  
          (else 
           (_ fn (- n 1) (cons (fn (- n 1)) l)))))
  (_ fn n '()))   
