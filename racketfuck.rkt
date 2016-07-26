#lang racket

(define memsize 30000) ;; Size of memory

;; Vector containing input file as single element
;; Change if not running from terminal
(define args (current-command-line-arguments))

;; Check for arguments
(if (not (= 1 (vector-length args)))
    (begin (print "Please provide a brainfuck-file name as the single argument")
           (exit))
    (display ""))


;; Read code from file
(define filename (vector-ref args 0))
(define code (string->list (file->string filename)))
(define symbols (list #\+
                      #\-
                      #\<
                      #\>
                      #\[
                      #\]
                      #\,
                      #\.))
(define instructions (list->vector (filter (lambda (x) (member x symbols)) code)))
(define ins-length (vector-length instructions))

;; Initialize memory
(define memory (make-vector memsize))

;; Helpers
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
; Searches for matching `]`
(define (seek-end insptr)
  (define (go insptr counter)
    (if (or (not (equal? (vector-ref instructions insptr) #\]))
            (not (= 0 counter)))
        (cond ((equal? (vector-ref instructions insptr) #\])
               (go (inc insptr) (dec counter)))
              ((equal? (vector-ref instructions (inc insptr)) #\[)
               (go (inc insptr) (inc counter)))
              (else
               (go (inc insptr) counter)))
        insptr))
  (go insptr 0))
; Searches for matching `[`
(define (seek-start insptr)
  (define (go insptr counter)
    (if (or (not (equal? (vector-ref instructions insptr) #\[))
            (not (= 0 counter)))
        (cond ((equal? (vector-ref instructions insptr) #\[)
               (go (dec insptr) (dec counter)))
              ((equal? (vector-ref instructions (dec insptr)) #\])
               (go (dec insptr) (inc counter)))
              (else
               (go (dec insptr) counter)))
        insptr))
  (go insptr 0))


;; interpretation method
;; insptr is position in instructions
;; memptr is position in memory
(define (run insptr memptr)
  (if (< insptr ins-length)
      (match (vector-ref instructions insptr)
        (#\+ (begin (vector-set! memory memptr (inc (vector-ref memory memptr)))
                    (run (inc insptr) memptr)))
        (#\- (begin (vector-set! memory memptr (dec (vector-ref memory memptr)))
                    (run (inc insptr) memptr)))
        (#\> (run (inc insptr) (inc memptr)))
        (#\< (run (inc insptr) (dec memptr)))
        (#\[ (run (if (= 0 (vector-ref memory memptr))
                      (inc (seek-end insptr))
                      (inc insptr))
                  memptr))
        (#\] (run (if (= 0 (vector-ref memory memptr))
                      (inc insptr)
                      (inc (seek-start insptr)))
                  memptr))
        (#\, (begin (vector-set! memory memptr (char->integer (read-char)))
                    (run (inc insptr) memptr)))
        (#\. (begin (display (integer->char (vector-ref memory memptr)))
                    (run (inc insptr) memptr))))
      (newline)))

;; Start interpretation
(run 0 0)