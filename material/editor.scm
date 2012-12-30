;-----------------------------
;the normal output ports, we'll insert our own before these
(define defport (current-output-port))
(define erport (current-error-port))

;keep track of what we have printed,mainly so we can get rid of it again
;also for scaling and appending
(define scene-messages (list ))
(define scene-messages-txt (list ))
(define last-message-time -1)
(define scene-messages-scale 1)


;the colours to print various message types in
(define error-colour (vector 10 0  0))
(define info-colour  (vector 0  10 0))
(define print-colour (vector 1  1  1))
(define debug-colour print-colour)


;get rid of the printed text again
(define (clear-print)
    (for-each (lambda (x) (when (number? x) (destroy x))) scene-messages)
    (set! scene-messages (list ))
    (set! scene-messages-txt (list ))
    (set! scene-messages-scale 1))

(clear-print)


;prints to the screen in letters as big as will fit
(define (print in-msg)
  (let ((output-list (list null)) (msg ""))
    (cond
     ((string? in-msg)     (set! msg in-msg))
     ((bytes? in-msg)      (set! msg (bytes->string/utf-8 in-msg)))
     ((number? in-msg)     (set! msg (number->string in-msg)))
     ((symbol? in-msg)     (set! msg (symbol->string in-msg)))
     ((boolean? in-msg)    (set! msg (if in-msg "#t" "#f")))
     (else
      (begin
        (error "type not supported by print")
        (set! msg ""))))
    

    
    (when (> (string-length msg) 0) ;we seem to be getting a length 0 message every frame
          
          (when (< last-message-time (time)) ;used to gather lines of the same message
                (clear-print)
                (set! last-message-time (time)))
          
                                        ;join the input with the last printed line, in case that line didn't end in a newline
          (when (not (null? scene-messages-txt) )
                (set! msg (string-append (car scene-messages-txt) msg))
                (set! scene-messages-txt (cdr scene-messages-txt))
                (when (number?(car scene-messages)) (destroy (car scene-messages)))
                (set! scene-messages (cdr scene-messages)))
          
                                        ;convert string to list, then sort that into a list of strings, one for every line to be printed
                                        ;also get rid of extra newline characters that aren't implicid in the list structure
          (let ((in-list (string->list msg)))
            (for ((x (in-range 0 (length in-list))))
                 (if (char=? (list-ref in-list x) #\newline)
                     (begin
                       (when (or (and (< x (- (length in-list) 1))
                                       (char=? (list-ref in-list (+ x 1)) #\newline))
                                 (eq? x (- (length in-list) 1)))
                             (set! output-list (cons (list #\newline ) output-list )))
                       (set! output-list (cons null output-list )))
                     (set! output-list (cons (append (car output-list) (list (list-ref in-list x))) (cdr output-list)))))
            (set! output-list (map list->string (reverse output-list))))
          
        ;print each line to build-type and scale the whole thing
        (for-each (lambda (output)
            (when (> (string-length output) 0)

                (set! scene-messages-scale
                    (min
                        scene-messages-scale
                        (/ .8 (string-length output))
                        (/ .4 (+ (length scene-messages) 1))
                        ))

                (with-state
                    (hint-ignore-depth)
                    (hint-depth-sort)
                    (hint-unlit)
                    (colour debug-colour)


                    (set! scene-messages
                        (cons (if (string=? output "\n")
                                    "\n"
                                    (build-type fluxus-scratchpad-font output))
                             scene-messages ))
                    (set! scene-messages-txt (cons output scene-messages-txt))

                    (for ((x (in-range 0 (length scene-messages))))
                         (when (number? (list-ref scene-messages x))
                               (with-primitive (list-ref scene-messages x)
                                               (identity)
                                               (concat (minverse (get-camera-transform)))
                                               (translate (vector -1.05 0 -1.1))
                                               (scale scene-messages-scale)
                                               (translate (vector 0 (+ (* -2 (length scene-messages)) (* 4 x) ) 0)))))
                    )))
                  output-list))))


 ; a port that prints to the scene graph

(define scene-port
      (make-output-port
       'scene-output-port
       always-evt
       (lambda (s start end non-block? breakable?)
               (set! debug-colour info-colour)
               (print (subbytes s start end))
               (set! debug-colour print-colour)
               (display (subbytes s start end) defport)


         (- end start))
       void))


(define scene-error-port
      (make-output-port
       'scene-error-port
       always-evt
       (lambda ( s start end non-block? breakable? )
             (set! debug-colour error-colour)
             (print (subbytes s start  end ))
             (set! debug-colour print-colour)
             (display (subbytes s start  end ) erport)


         (- end start))
       void))

(current-output-port scene-port)
(current-error-port scene-error-port)

(clear)
(define all_characters (string->list "abcdefghijklmnopqrstuvwxyz 1234567890()[]{}+-=<>/!@#$%^&*|"))
(define my_text "")

(define (eval-string input)
  (eval (read (open-input-string input))))

(define (keyb-out)
  (let ((output null))
    (map (lambda (x) (when (key-pressed-this-frame (string x))
                           (set! output (cons (string x) output))))
         all_characters)
    output))

(define (typing)
  (let ((newchars (keyb-out)))
    (for ((x (in-range 0 (length newchars))))
         (set! my_text (string-append my_text (list-ref newchars x)))))
  (when (and (key-pressed-this-frame (string #\backspace))
             (> (string-length my_text) 0))
        (set! my_text (substring my_text 0 (- (string-length my_text) 1))))
  (when (key-pressed-this-frame (string #\return))
        (set! my_text (string-append my_text "\n")))
  (when (key-special-pressed-this-frame 1) (eval-string my_text))
  (if (> (string-length my_text) 0)
      (print my_text)
      (clear-print)
      ))

(every-frame
 (typing))

