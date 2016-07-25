#lang racket

(define args (current-command-line-arguments))

;(define args '#("helloworld.bf"))
; Useful for testing 

(if (not (= 1 (vector-length args)))
    (begin (print "Please provide a brainfuck-file name as the single argument")
           (exit))
    (display ""))

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

(set! code (filter (lambda (x) (member x symbols)) code))
