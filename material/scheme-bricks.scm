;; [ Copyright (C) 2011 Dave Griffiths : GPLv2 see LICENCE ]

(clear)

(translate (vector -10 -10 0))

(define (insert-to i p l)
  (cond
   ((null? l) (list i))
   ((zero? p) (cons i l))
   (else
    (cons (car l) (insert-to i (- p 1) (cdr l))))))

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (text-from-code code)
   (cond 
    ((string? code) 
     (string-append "\"" code "\""))
    ((number? code) 
     (number->string code))
    ((symbol? code) 
     (symbol->string code))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (make-brick text children)
  (list text children
        (let ((prim (with-state
                     (build-polygons 8 triangle-strip))))
          (with-primitive 
           prim
           (hint-unlit)
           ;(hint-none)(hint-wire)
           (pdata-set! "p" 0 (vector 5 0 0))
           (pdata-set! "p" 1 (vector 5 1 0))
           (pdata-set! "p" 2 (vector 0 0 0))
           (pdata-set! "p" 3 (vector -1 1 0))
           (pdata-set! "p" 4 (vector 0 0 0))
           (pdata-set! "p" 5 (vector -1 -1 0))
           (pdata-set! "p" 6 (vector 5 0 0))
           (pdata-set! "p" 7 (vector 5 -1 0))                      
           (pdata-map!
            (lambda (n)
              (vector 0 0 1))
            "n")
           (pdata-map!
            (lambda (c)
              (vector 1 1 1))
            "c")
           (apply-transform)
    ;       (scale (vector 0.4 0.4 0.4))
           (pdata-copy "p" "pref"))
          prim)))

(define (brick-text b) (list-ref b 0))
(define (brick-modify-text f b) (list-replace b 0 (f (brick-text b))))
(define (brick-children b) (list-ref b 1))
(define (brick-is-atom? b) (not (brick-children b)))
(define (brick-modify-children f b) (list-replace b 1 (f (brick-children b))))
(define (brick-prim b) (list-ref b 2))

(define (brick-expand! b n)
  (with-primitive 
   (brick-prim b) 
   (pdata-set! "p" 4 (vadd (pdata-ref "pref" 4) (vector 0 (- n) 0)))
   (pdata-set! "p" 5 (vadd (pdata-ref "pref" 5) (vector 0 (- n) 0)))
   (pdata-set! "p" 6 (vadd (pdata-ref "pref" 6) (vector 0 (- n) 0)))
   (pdata-set! "p" 7 (vadd (pdata-ref "pref" 7) (vector 0 (- n) 0)))))

(define (make-brick-from-atom code)
  (make-brick (text-from-code code) #f))

(define (code->brick code)
  (cond 
   ((not (list? code)) (make-brick-from-atom code))
   (else
    (make-brick  
     ""
     (map
      (lambda (item)
        (code->brick item))
      code)))))

(define (brick-children-size b)
  (if (not (brick-is-atom? b))
      (foldr
       (lambda (n child)
         (+ n (brick-size child)))
       0
       (brick-children b))
      0))

(define (brick-size b)
  (+ 2 (brick-children-size b)))

(define (brick->text b)
  (string-append "(" (brick-text b) " "
                 (if (not (brick-is-atom? b))
                     (apply string-append
                            (map
                             (lambda (child)
                               (string-append 
                                (brick->text child) " "))
                             (brick-children b)))
                     "")
                 ")"))

(define (brick-dock b new pos)
    (brick-modify-children
     (lambda (children)
       (insert-to new pos children))
     b))

(define (brick-undock b id)
    (brick-modify-children
     (lambda (children)
       (filter
        (lambda (b)
          (not (eqv? (brick-prim b) id)))
        children))
     b))

; update the primitive and children to match the state
(define (brick-update! b d)
  (with-primitive 
   (brick-prim b)
   (pdata-map!
    (lambda (c)
      (vector 1 (/ (modulo d 6) 6) (/ (modulo d 4) 4)))
    "c"))
  (when (not (brick-is-atom? b))
        (let ((size
               (foldr
                (lambda (p child)
                  (with-primitive 
                   (brick-prim child)
                   (identity)
                   (parent (brick-prim b))
                   (translate (vector 1 (- p) 0)))
                  (brick-update! child (+ d 1)) ;
                  (+ p (brick-size child)))  ;
                1
                (brick-children b))))
          (brick-expand! b (- size 1)))))

(define pointer (with-state 
                 (colour (vector 1 0 0)) 
                 (scale (vector 0.1 0.1 0.1)) 
                 (build-cube)))

(define (brick-intersect b line)
  (with-primitive 
   (brick-prim b)
   (let ((p (geo/line-intersect (vtransform (car line) (minverse (get-global-transform)))
                                (vtransform (cadr line) (minverse (get-global-transform)))))
         (m (get-global-transform)))
     (if (not (null? p))
         (begin
           (with-primitive 
            pointer
            (identity)
            (translate (vtransform (cdr (assoc "p" (car p))) m)))
           b)
         (if (not (brick-is-atom? b)) 
             (foldr
              (lambda (r child)
                (if (not r) (brick-intersect child line) r))
              #f
              (brick-children b))
             #f)))))

;---------------------------------------------------------  

(define (make-bricks)
  (list '() (vector 0 0 0) #f #f))

(define (bricks-roots b) (list-ref b 0))
(define (bricks-modify-roots f b) (list-replace b 0 (f (bricks-roots b))))
(define (bricks-mouse b) (list-ref b 1))
(define (bricks-modify-mouse f b) (list-replace b 1 (f (bricks-mouse b))))
(define (bricks-button b) (list-ref b 2))
(define (bricks-modify-button f b) (list-replace b 2 (f (bricks-button b))))
(define (bricks-current b) (list-ref b 3))
(define (bricks-modify-current f b) (list-replace b 3 (f (bricks-current b))))

(define (bricks-add-code b code)
  (bricks-modify-roots
   (lambda (roots)
     (cons (code->brick code) roots))
   b))

(define (bricks-get-over b pos)
  (let ((line (get-line-from-mouse)))
    (foldr
     (lambda (r b)
       (if (not r) (brick-intersect b line) r))
     #f
     (bricks-roots b))))

(define (bricks-update! b)
  (let* ((pos (get-point-from-mouse)))
    (when
    ; update bricks when mouse button is held down
     (or (mouse-button 0) (bricks-button b))
      (for-each
       (lambda (b)
         (brick-update! b 0))
       (bricks-roots b)))

    ; move the current brick
    (when (list? (bricks-current b))
        (with-primitive 
         (brick-prim (bricks-current b))
         (when (and (mouse-button 0) (bricks-button b))
               (translate (vsub (bricks-mouse b) pos)))
         (pdata-map! (lambda (c) (vector 0 0 1)) "c")))

    ; update the input stuff
    (bricks-modify-mouse
     (lambda (m)
       pos)
     (bricks-modify-button
      (lambda (b)
        (mouse-button 0))
      (bricks-modify-current
       (lambda (current)
       ; when clicked
         (if (and (mouse-button 0) (not (bricks-button b)))
             (bricks-get-over b pos) 
             current))
       ;
       (if (list? bricks-current)
           (bricks-modify-roots
            (lambda (roots)
              (when (and (mouse-button 0) (bricks-button b))
                    (translate (vsub (bricks-mouse b) pos)))
              (pdata-map! (lambda (c) (vector 0 0 1)) "c")))
           b))))))

;---------------------------------------------------------  

(define b (make-bricks))
(set! b (bricks-add-code b '("wee" (((()())(("we")))("we"("we"))))))
(set! b (bricks-add-code b '("wee" ())))

(define (setup b loc)
  (with-primitive 
   (brick-prim b)
   (translate loc))
  (brick-update! b 0))

(setup (car (bricks-roots b)) (vector -15 23 0))
(setup (cadr (bricks-roots b)) (vector 5 10 0))

(clear-colour (vector 0.5 0.2 0.1))
(define t (with-state 
           ;(hint-none) 
           ;(hint-wire)
           ;(hint-unlit)
           ;(line-width 5)
           (scale (vector 4 4 4))
           (colour (vector 1 1 1)) (build-cube)))

(with-primitive 
 t
 (pdata-map!
  (lambda (c)
    (vector 0.5 0.5 1))
  "c"))

(every-frame 
 (begin
   (set! b (bricks-update! b))
   (with-primitive t (rotate (vector 1 2 3)))))
