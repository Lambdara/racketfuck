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

;; Run loop balance check
(define (check code balance)
  (if (or (null? code) (< balance 0))
      (= balance 0)
      (check (cdr code)
             (match (car code)
               (#\[ (add1 balance))
               (#\] (sub1 balance))
               (_ balance)))))
(if (check code 0)
    (values)
    (begin (print "Balance check failed, please match `[` and `]`")
           (newline)
           (exit)))

;; Put instrunctions in vector
(define instructions (list->vector (filter (lambda (x) (member x symbols)) code)))
(define ins-length (vector-length instructions))

;; Initialize memory
(define memory (make-vector memsize))

;; Searches for matching `]`
(define (seek-end insptr)
  (define (go insptr counter)
    (if (or (not (equal? (vector-ref instructions insptr) #\]))
            (not (= 0 counter)))
        (cond ((equal? (vector-ref instructions insptr) #\])
               (go ( insptr) ( counter)))
              ((equal? (vector-ref instructions (add1 insptr)) #\[)
               (go (add1 insptr) (add1 counter)))
              (else
               (go (add1 insptr) counter)))
        insptr))
  (go insptr 0))
;; Searches for matching `[`
(define (seek-start insptr)
  (define (go insptr counter)
    (if (or (not (equal? (vector-ref instructions insptr) #\[))
            (not (= 0 counter)))
        (cond ((equal? (vector-ref instructions insptr) #\[)
               (go (sub1 insptr) (sub1 counter)))
              ((equal? (vector-ref instructions (sub1 insptr)) #\])
               (go (sub1 insptr) (add1 counter)))
              (else
               (go (sub1 insptr) counter)))
        insptr))
  (go insptr 0))


;; interpretation method
;; insptr is position in instructions
;; memptr is position in memory
(define (run insptr memptr)
  (if (< insptr ins-length)
      (match (vector-ref instructions insptr)
        (#\+ (begin (vector-set! memory memptr (add1 (vector-ref memory memptr)))
                    (run (add1 insptr) memptr)))
        (#\- (begin (vector-set! memory memptr (sub1 (vector-ref memory memptr)))
                    (run (add1 insptr) memptr)))
        (#\> (run (add1 insptr) (add1 memptr)))
        (#\< (run (add1 insptr) (sub1 memptr)))
        (#\[ (run (if (= 0 (vector-ref memory memptr))
                      (add1 (seek-end insptr))
                      (add1 insptr))
                  memptr))
        (#\] (run (if (= 0 (vector-ref memory memptr))
                      (add1 insptr)
                      (add1 (seek-start insptr)))
                  memptr))
        (#\, (begin (vector-set! memory memptr (char->integer (read-char)))
                    (run (add1 insptr) memptr)))
        (#\. (begin (display (integer->char (vector-ref memory memptr)))
                    (run (add1 insptr) memptr))))
      (exit)))

;; Start interpretation
(run 0 0)