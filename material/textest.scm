(clear-colour (vector 0 0 1))
(colour (vector 1 1 1))
(texture (load-texture "font.png"))
(hint-unlit)
(define x (build-cube))

(set! frame-thunk
      (lambda ()
        (grab x)
        (rotate (vector .2 .4 .5))
        (ungrab)))

;(clear)


;(define q (with-state
;           (build-cube)
           ;(hint-none)
           ;(hint-wire)
;           (translate (vector 0 2 0))
;           (colour (vector 1 0 0))
;           (build-cube)))


;(every-frame
; (with-primitive 
;  q
;  (rotate (vector 0 1 0))))

