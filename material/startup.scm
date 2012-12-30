(clear)

(define t (with-state
           (texture (load-texture "font.png"))
           (hint-unlit)
           (colour (vector 1 1 1))
           (translate (vector -1 0.5 8))
           (scale (vector 0.07 0.07 1))
;           (hint-ignore-depth) 
;           (hint-no-zwrite) 
           (build-text "                                                                                                                                                                                                    ")))

;(clear-colour (vector 1 1 1))

(define (print msg)
  (with-primitive 
   t (set-text msg)))

(define all_characters (string->list "abcdefghijklmnopqrstuvwxyz 1234567890()[]{}+-=<>/!@#$%^&*|"))
(define my_text "
(define (build n)
  (when (not (zero? n))
    (rotate (vector 45 45 40))
    (with-state
      (scale (vector 3 0.1 0.1))
      (build-cube))
    (build (- n 1))))

(build 20)")

(define (eval-string input)
  (pre-process-run (read (open-input-string (string-append "(" input ")")))))

(define (keyb-out)
  (let ((output '()))
    (map (lambda (x)
           (when (key-pressed-this-frame (string x))
                 (set! output (cons (string x) output))))
         all_characters)
    output))

(define (typing)
  (let ((newchars (keyb-out)))
    (for-each
     (lambda (x)
       (display x)(newline)
       (set! my_text (string-append my_text x)))
     newchars))
  
  (when (and (key-pressed-this-frame (string #\backspace))
             (> (string-length my_text) 0))
        (set! my_text (substring my_text 0 (- (string-length my_text) 1))))
  (when (key-pressed-this-frame (string #\return))
        (set! my_text (string-append my_text "\n")))
  (when (key-special-pressed-this-frame 1) 
        (eval-string my_text))
  (when (> (string-length my_text) 0)
      (print my_text)
      ))

(every-frame
 (typing))

