;; al jazari two (c) 2013 dave griffiths gpl v3

(define octree-size 32)
(define octree-depth 5)

(define (make-octree v) (vector v v v v v v v v))
(define (make-empty-octree) (make-octree 'e))
(define (octree-replace o i v) (vector-set! o i v) o)
(define octree-branch vector-ref)
(define (octree-leaf? o i) (not (vector? (vector-ref o i))))
(define (octree-empty? o) (equal? o (vector 'e 'e 'e 'e 'e 'e 'e 'e)))
(define (octree-contig? o) 
  (let ((v (octree-branch o 0)))
    (and
     (eq? (octree-branch o 1) v)
     (eq? (octree-branch o 2) v)
     (eq? (octree-branch o 3) v)
     (eq? (octree-branch o 4) v)
     (eq? (octree-branch o 5) v)
     (eq? (octree-branch o 6) v)
     (eq? (octree-branch o 7) v))))

(define (octree-path pos)
  (define (_ x y z depth)
    (cond 
      ((eq? depth octree-depth) '())
      (else
       (let ((split (/ octree-size (expt 2 (+ depth 1)))))
         (cons
          (bitwise-ior
           (if (< x split) 0 1)
           (if (< y split) 0 2)
           (if (< z split) 0 4))
          (_ (if (>= x split) (- x split) x)
             (if (>= y split) (- y split) y)
             (if (>= z split) (- z split) z) (+ depth 1)))))))
  (_ (vector-ref pos 0)
     (vector-ref pos 1)
     (vector-ref pos 2) 0))

(define (octree-ref o pos)
  (define (_ o path)
    (cond
      ((octree-leaf? o (car path)) 
       (octree-branch o (car path)))
      (else
       (_ (octree-branch o (car path)) (cdr path)))))
  (_ o (octree-path pos)))

(define (octree-set o pos v)
  (define (_ o path v depth)
    (cond
      ;; at bottom of the tree?
      ((or
        (null? path)
        (eq? depth octree-depth))
       v)
      ;; reached a collapsed tree?
      ((octree-leaf? o (car path))
       (let ((value (octree-branch o (car path))))
         (octree-replace
          o (car path)
          (_ (make-octree value)
             (cdr path) v (+ depth 1)))))
      ;; drill down existing path
      (else
       (octree-replace
        o (car path)
        (_ (octree-branch o (car path))
           (cdr path) v (+ depth 1))))))
  (_ o (octree-path pos) v 0))
       
(define (octree-delete o pos)
  (octree-set o pos 'e))

;; collapse chunks into single values, empty space or solid objects
(define (octree-compress o)
  (define (_ o)
    (cond
      ((not (vector? o)) o) ;; it's a leaf
      (else
       (let ((r (vector
                 (_ (octree-branch o 0))
                 (_ (octree-branch o 1))
                 (_ (octree-branch o 2))
                 (_ (octree-branch o 3))
                 (_ (octree-branch o 4))
                 (_ (octree-branch o 5))
                 (_ (octree-branch o 6))
                 (_ (octree-branch o 7)))))
         (if (octree-contig? r) (octree-branch o 0) r)))))
  (_ o))

(define (print-tab s)
  (for-each (lambda (_) (display "-")) (build-list s (lambda (_) 0))))

(define (octree-print o)
  (define (_ o depth)
    (cond
      ((eq? o 'e) 0)
      ((not (vector? o)) (print-tab depth)(display o)(newline))
      (else
       (print-tab depth)(newline)
       (_ (octree-branch o 0) (+ depth 1))
       (_ (octree-branch o 1) (+ depth 1))
       (_ (octree-branch o 2) (+ depth 1))
       (_ (octree-branch o 3) (+ depth 1))
       (_ (octree-branch o 4) (+ depth 1))
       (_ (octree-branch o 5) (+ depth 1))
       (_ (octree-branch o 6) (+ depth 1))
       (_ (octree-branch o 7) (+ depth 1)))))
  (_ o 0))

(define (octree-render f o)
  (define (_ o x y z depth)
    (let ((s (/ octree-size (expt 2 depth) 2)))
      (cond
        ((eq? o 'e) 0)
        ((not (vector? o)) (f (vector x y z) (* s 2) o))
        (else
         (_ (octree-branch o 0) x y z (+ depth 1))
         (_ (octree-branch o 1) (+ x s) y z (+ depth 1))
         (_ (octree-branch o 2) x (+ y s) z (+ depth 1))
         (_ (octree-branch o 3) (+ x s) (+ y s) z (+ depth 1))
         (_ (octree-branch o 4) x y (+ z s) (+ depth 1))
         (_ (octree-branch o 5) (+ x s) y (+ z s) (+ depth 1))
         (_ (octree-branch o 6) x (+ y s) (+ z s) (+ depth 1))
         (_ (octree-branch o 7) (+ x s) (+ y s) (+ z s) (+ depth 1))))))
    (_ o 0 0 0 0))

(define (index->coords i)
  (vector
   (modulo i octree-size)
   (modulo (quotient i octree-size) octree-size)
   (quotient i (* octree-size
                  octree-size))))

(define (vdist a b)
  (let ((vx (- (vector-ref b 0) (vector-ref a 0)))
        (vy (- (vector-ref b 1) (vector-ref a 1)))
        (vz (- (vector-ref b 2) (vector-ref a 2))))    
    (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))

(define (octree-fold o fn)
  (letrec ((i -1)
           (l (* octree-size
                 octree-size 
                 octree-size))
           (_ (lambda (o)
                (set! i (+ i 1))
                (if (< i l) (_ (fn o (index->coords i))) o))))
    (_ o)))

(define (octree-cut o y)
  (octree-fold 
   o 
   (lambda (o pos)
     (if (> (vector-ref pos 2) y)
         (octree-delete o pos)
         o))))

(define (inside? pos min max)
  (and
   (>= (vector-ref pos 0) (vector-ref min 0))
   (< (vector-ref pos 0) (vector-ref max 0))
   (>= (vector-ref pos 1) (vector-ref min 1))
   (< (vector-ref pos 1) (vector-ref max 1))
   (>= (vector-ref pos 2) (vector-ref min 2))
   (< (vector-ref pos 2) (vector-ref max 2))))

(define (octree-box o min max v)
  (octree-fold
   o
   (lambda (o pos)
     (if (inside? pos min max)
         (octree-set o pos v)
         o))))

(define (octree-delete-box o min max)
  (octree-fold
   o
   (lambda (o pos)
     (if (inside? pos min max)
         (octree-delete o pos)
         o))))

(define (octree-fill-sphere o pos size v)
  (octree-fold
   o
   (lambda (o ipos)
     (if (< (vdist ipos pos) size)
         (octree-set o ipos v)
         o))))

(define (octree-delete-sphere o pos size)
  (octree-fold
   o
   (lambda (o ipos)
     (if (< (vdist ipos pos) size)
         (octree-delete o ipos)
         o))))

(define (test)
  (define o (make-empty-octree))
  
  (octree-ref o 30 30 13)
  ;(set! o (octree-set o 30 30 13 '111))
  ;(set! o (octree-set o 0 30 133 '222))
  (set! o (octree-set o 230 235 13 '333))
  (set! o (octree-set o 30 30 14 '444))
  ;(set! o (octree-set o 30 31 14 '555))
  
  
  (octree-ref o 30 30 14)
  (set! o (octree-delete o 30 30 14))
  (set! o (octree-delete o 230 235 13))
  (octree-ref o 30 30 14)
  
  (octree-print o)
  (display o)(newline)
  (set! o (octree-compress o))
  (display o)(newline))
  
