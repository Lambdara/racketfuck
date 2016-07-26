#lang racket

(define memsize 30000) ;; Size of memory


;(define args (current-command-line-arguments))

(define args '#("helloworld.bf"))
; Useful for testing 


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

;; Initialize memory
(define memory (make-vector memsize))

;; Helpers
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

;; insptr is position in instructions
;; memptr is position in memory
(define (run insptr memptr)
  (match (vector-ref instructions insptr)
    (#\+ (begin (vector-set! memory memptr (inc (vector-ref memory memptr)))
                (run (inc insptr) memptr)))
    (#\- (begin (vector-set! memory memptr (dec (vector-ref memory memptr)))
                (run (inc insptr) (memptr))))
    (#\> (run (inc insptr) (inc memptr)))
    (#\< (run (inc insptr) (dec memptr)))
    (#\[ (run (seek-end insptr) memptr))
    (#\] (run (seek-start insptr) memptr))
    (#\, (begin (vector-set! memory memptr (char->integer (read-char)))
                (run (inc insptr) memptr)))
    (#\. (begin (display (integer->char (vector-ref memory memptr)))))))