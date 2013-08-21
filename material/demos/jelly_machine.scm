(define nop 0)
(define jmp 1)
(define jmz 2)
(define jlt 3)
(define jgt 4)
(define ldl 5)
(define lda 6)
(define ldi 7)
(define sta 8)
(define sti 9)
(define add 10)
(define sub 11)
(define mul 12)
(define div 13)
(define abs 14)
(define sin 15)
(define atn 16)
(define dot 17)
(define crs 18)
(define sqr 19)
(define len 20)
(define dup 21)
(define cmp 22)
(define shf 23)
(define bld 24)
(define ret 25)
(define end-check 999)

(define reg-pco 100)
(define reg-spd 101)
(define reg-pos 102)
(define reg-vel 103)
(define reg-col 104)
(define reg-nit 105)
(define reg-scc 106)
(define reg-sdr 107)
(define reg-and 108)
(define reg-ins 109)
(define reg-mdl 120)
(define reg-mdl-end 199)
(define reg-stp 200)
(define reg-stk 201)
(define reg-ndt 256)

(define (jelly-prog . args)
  (display args)(newline)
  (let* ((ins-per-frame (car args))
         (l (cdr args))
         (p (foldl
             (lambda (i r)
               (let ((cur (car r))
                     (l (cadr r)))
                 (if (eq? (length cur) 3)
                     (list (list i) (append l (list (list->vector cur))))
                     (list (append cur (list i)) l))))
             (list '() '()) l)))
    (cond
     ((eq? (car (car p)) end-check)
      (pdata-set! "p" reg-ins (vector ins-per-frame 0 0))
      (define addr 0)
      (for-each
       (lambda (v)
         (display v)(newline)
         (pdata-set! "p" addr v)
         (set! addr (+ addr 1)))
       (cadr p)))
     (else (display "end check wrong ")(display p)(newline)))))

(clear)
(clear-colour (vector 1 1 1))
;(define p (build-cube))
(define jelly (build-jellyfish))
(display "hello")(newline)

(with-primitive
 jelly
 (jelly-prog
  10000
  0 0 0   ;; time (increases by 1 each loop)
  2 2 -3  ;; shuffle data for converting (x y z) -> (z z x)
  ;; code follows to build a vertex by rotation around an angle based on the index
  lda 0 0       ;; load current time from address 0
  ldl 135.3 0   ;; load angle 135.3 (in degrees)
  mul 0 0       ;; multiply time by angle
  sin 0 0       ;; makes (sin(angle) cos(angle) 0)
  ;; make a spiral by scaling up with time
  lda 0 0       ;; load time again
  ldl 0.05 0    ;; load 0.05
  mul 0 0       ;; multiply to get time*0.05
  mul 0 0       ;; mul rotation vector by time*0.05
  ;; move backward in z so we get some depth
  lda 0 0       ;; load the time again
  ldl 0.03 0    ;; load 0.03
  mul 0 0       ;; multiply the time by 0.01
  lda 1 0       ;; load the shuffle vec from address 1
  shf 0 0       ;; shuffle the x to z position
  add 0 0       ;; add (0 0 x) to set z on current position
  sti 0 reg-mdl ;; write position to model memory registers
  ;; increment the index by 1
  lda 0 0       ;; load address
  ldl 1 0       ;; load inc
  add 0 0       ;; add them together
  sta 0 0       ;; store at address loc
  jmp 2 0       ;; goto 2
  end-check)

 (pdata-map!
  (lambda (c)
    (rndvec)) ;(vector 0.7 0.7 0.7 0.4))
  "c")
 (hint-unlit)
 (hint-wire)
 (line-width 3)
 )


(with-primitive
 jelly
 (translate (vector -0.3 1.4 0))
 (rotate (vector -40 -20 2))
 )

                                        ;(with-state
; (rotate (vector 45 45 0))
; (build-cube))

;(every-frame
; (with-primitive jelly (rotate (vector 2.2 2 1))))
