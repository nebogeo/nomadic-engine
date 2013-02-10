(define _mouse-x 0)
(define _mouse-y 0)
(define _mouse-b -1)
(define _mouse-s 1) ; state - 0 down, 1 up

(define (input-mouse-button b s)
  (set! _mouse-b b)
  (set! _mouse-s s))

(define (input-mouse x y)
  (when (zero? _mouse-s) ; eh?
        (set! _touching #t)
        (set! _touches (list (list 0 x y))))
  (set! _mouse-x x)
  (set! _mouse-y y))

(define (mouse-x) _mouse-x)
(define (mouse-y) _mouse-y)
(define (mouse-button n)
  (if _touching
      #t
      (if (zero? _mouse-s) 
          (eqv? _mouse-b n) #f)))

(define keys '())
(define keys-this-frame '())
(define special-keys '())
(define special-keys-this-frame '())
(define mouse (vector 0 0))
(define mouse-buttons (vector #f #f #f))
(define mouse-wheel-v 0)
(define key-mods '())



(define (register-down key button special state x y mod)
  (when (not (or (number? key) (eqv? key -1))) ; ordinary keypress
    (set! keys (set-add key keys))
	(set! keys-this-frame (set-add key keys-this-frame)))
  (when (not (eqv? special -1)) ; special keypress
    (set! special-keys (set-add special special-keys))
	(set! special-keys-this-frame (set-add special special-keys-this-frame)))
  ;(set! key-mods ; key modifiers
 ;	  (for/list ([bitmask (list 1 2 4)]
;				 [bitsym '(shift ctrl alt)]
;				 #:when (> (bitwise-and mod bitmask) 0))
;			bitsym))
  ;(cond ; mouse
  ;  ((and (eq? key 0) (eq? special -1)) 
;	 (when (eq? button 3) (set! mouse-wheel-v 1))
;	 (when (eq? button 4) (set! mouse-wheel-v -1))
;     (when (and (eq? state 0)
;				(< button (vector-length mouse-buttons)))
;	   (vector-set! mouse-buttons button #t))
 ;    (when (and (eq? state 1)
;				(< button (vector-length mouse-buttons)))
;	   (vector-set! mouse-buttons button #f))
 ;    (vector-set! mouse 0 x)
  ;   (vector-set! mouse 1 y)))
  )

(define (register-up key button special state x y mod)
  (display keys)(newline)
  (when (not (eqv? key -1))
    (set! keys (set-remove key keys)))
  (when (not (eqv? special -1))
    (set! special-keys (set-remove special special-keys))))

(define (key-pressed s)
  (set-contains (car (string->list s)) keys))

(define (keys-down)
  keys)

(define (key-special-pressed k)
  (set-contains k special-keys))

(define (keys-special-down)
  special-keys)

(define (key-modifiers)
  key-mods)

(define (key-pressed-this-frame s)
  (set-contains (car (string->list s)) keys-this-frame))

(define (key-special-pressed-this-frame s)
  (set-contains s special-keys-this-frame))

(define (fluxus-input-callback key button special state x y mod)
  (register-down key button special state x y mod)
  ;(input-camera key button special state x y mod width height)
  )

(define (fluxus-input-release-callback key button special state x y mod)
  (register-up key button special state x y mod))

;------------------------------------------------------------

(define _touching #f)
(define _touches '())

(define (input-touches l)
  (set! _touching #t)
  (input-mouse (list-ref (car l) 1)
               (list-ref (car l) 2))
  (set! _touches l))

(define (get-touch-ids)
  (map
   (lambda (touch)
     (car touch))
   _touches))

(define (get-pos-from-touch id)
  (foldr
   (lambda (r touch)
     (if (eq? (car touch) id)
         (cdr touch)
         r))
   '(0 0)
   _touches))

;------------------------------------------------------------

(define (get-line-from-screen x y)
  (let* ((ndcpos (vector (* (- (/ x (vx (get-screen-size))) 0.5) 3.5 0.8)
                         (* (- (- (/ y (vy (get-screen-size))) 0.5)) 4 0.8) 10))
         (scrpos2 (vtransform (vmul ndcpos 500) (get-camera-transform)))
         (scrpos (vtransform ndcpos (get-camera-transform))))
    (list scrpos scrpos2)))

(define (get-point-from-screen x y)
  (let ((line (get-line-from-screen x y)))
;    (display (car line)) (display "->") (display (cadr line))(newline)
    (let ((v (vlerp (car line) (cadr line) 0.008)))
;      (display v)(newline)
      v)))

(define (get-line-from-mouse)
  (get-line-from-screen (mouse-x) (mouse-y)))

(define (get-point-from-mouse)
  (get-point-from-screen (mouse-x) (mouse-y)))

(define (get-line-from-touch id)
  (let ((pos (get-pos-from-touch id)))
    (get-line-from-screen (car pos) (cadr pos))))

(define (get-point-from-touch id)
  (let ((pos (get-pos-from-touch id)))
    (get-point-from-screen (car pos) (cadr pos))))

