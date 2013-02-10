
(define triangles 0)
(define triangle-strip 1)

(define (hint-none) (hint 0)) 
(define (hint-solid) (hint 1)) 
(define (hint-wire) (hint 2)) 
(define (hint-normal) (hint 3)) 
(define (hint-points) (hint 4)) 
(define (hint-anti-alias) (hint 5)) 
(define (hint-bound) (hint 6)) 
(define (hint-unlit) (hint 7)) 
(define (hint-ignore-depth) (hint 11)) 
(define (hint-no-zwrite) (hint 20)) 

(define flx_time 0)
(define (time) flx_time)
(define frame-thunk '())

(define (build-locator)
  (build-polygons 0 0))

(define (frame-hook)
  (set! flx_time (+ flx_time 1))
  (if (not (null? frame-thunk))
      (frame-thunk))  
  (set! _touches '())
  (set! _touching #f)
  (set! keys-this-frame '())
  (set! special-keys-this-frame '()))

;(define-macro (with-state . args) 
;  `(begin (push) (let ((r (begin ,@args))) (pop) r)))

;(define-macro (with-primitive . args) 
;  (let ((id (car args)) (body (cdr args)))
;    `(begin (grab ,id) (let ((r (begin ,@body))) (ungrab) r))))

(define (pdata-map! . args)
  (let ((proc (car args))
        (pdata-write-name (cadr args))
        (pdata-read-names (cddr args)))
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set! 
                          pdata-write-name n
                          (apply
                           proc 
                           (cons
                            (pdata-ref pdata-write-name n)
                            (map
                             (lambda (read)
                               (pdata-ref read n))
                             pdata-read-names))))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1)))))

(define (pdata-index-map! . args)
  (let ((proc (car args))
        (pdata-write-name (cadr args))
        (pdata-read-names (cddr args)))
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set! 
                          pdata-write-name n
                          (apply
                           proc 
                           (append
                            (list
                             n
                             (pdata-ref pdata-write-name n))
                            (map
                             (lambda (read)
                               (pdata-ref read n))
                             pdata-read-names))))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1)))))

(define (pdata-copy a b)
  (pdata-add b)
  (pdata-map! (lambda (b a) a) b a))

; ------------------------------------------------------

(define (do-with-state a)
  (list 'begin '(push) 
        (list 'let (list (list 'r (cons 'begin a))) 
              '(pop) 'r)))

(define (do-with-primitive a)
  (list 'begin (list 'grab (car a)) 
        (list 'let 
              (list (list 'r (cons 'begin (cdr a)))) 
              '(ungrab) 'r)))

(define (do-every-frame a)
  (list 'set! 'frame-thunk (append (list 'lambda '()) a)))
