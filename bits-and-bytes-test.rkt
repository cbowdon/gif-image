#lang racket

(require rackunit
         rackunit/text-ui
         "bits-and-bytes.rkt")

(define/provide-test-suite
  
  bits-and-bytes
  
  (test-case
   "Test byte->bits with some known values"
   (check-equal? (byte->bits 255) '(1 1 1 1 1 1 1 1))
   (check-equal? (byte->bits 0) '(0 0 0 0 0 0 0 0))
   (check-equal? (byte->bits 231) '(1 1 1 0 0 1 1 1))
   (check-equal? (byte->bits 247) '(1 1 1 0 1 1 1 1))
   (check-equal? (byte->bits 128) '(0 0 0 0 0 0 0 1)))
  
  (test-case
   "Test bits->byte with some known values"
   (check-equal? (bits->byte '(1 1 1 1 1 1 1 1)) 255)
   (check-equal? (bits->byte '(0 0 0 0 0 0 0 0)) 0)
   (check-equal? (bits->byte '(1 1 1 0 0 1 1 1)) 231)
   (check-equal? (bits->byte '(1 1 1 0 1 1 1 1)) 247)
   (check-equal? (bits->byte '(0 0 0 0 0 0 0 1)) 128))
  
  (test-case
   "Loop through 0 to 255 and check agreement"
   (define (loop-test x)
     (if [< x 256]
         (begin (check-equal? (bits->byte (byte->bits x)) x)
                (loop-test (+ x 1)))
         #t))
   (loop-test 0))
  
  ; not really req'd?
  (test-case
   "Test bytes->short with some known values"
   (check-equal? (bytes->short (bytes 0 0)) 0)
   (check-equal? (bytes->short (bytes 9 0)) 9)
   (check-equal? (bytes->short (bytes 0 1)) 256))
  
  (test-case
   "Test bytes->coord with some known values"
   (check-equal? (bytes->coord (bytes 0 0 0 0)) (cons 0 0))))
