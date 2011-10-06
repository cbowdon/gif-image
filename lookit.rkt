#lang racket

(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")
(define my "images/my.gif")

(define (get-bytes f) 
  (call-with-input-file f
    (lambda (in) (read-bytes (file-size f) in))))

(define (display-bytes x)
 (for-each displayln (bytes->list x)))

(define (show-bytes f)
  (display-bytes (get-bytes f)))