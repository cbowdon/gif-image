#lang racket

(provide gif?
         version
         gct-size
         trailer?
         img?
         img-size
         gif:)

(require "bits-and-bytes.rkt")

(define (gif? filename)
  (call-with-input-file filename
    (lambda (in) (equal? (read-bytes 3 in) #"GIF"))))

(define (version in)
  (peek-string 3 3 in))

(define (gct-size in)
  (let* ([packed-field (byte->bits (peek-byte in 10))]
         [i (+ 1 
               (* 4 (sixth packed-field)) 
               (* 2 (seventh packed-field))
               (eighth packed-field))])
    (expt 2 i)))

(define (trailer? in byte) 
  (let ([eof-0 (peek-byte in byte)]
        [eof-1 (peek-byte in (+ byte 1))])
    (and 
     (equal? eof-0 59)
     (equal? eof-1 eof))))

(define (img? in byte)
  (let* (; preceding byte (should be termination byte: 0)
         [id-m1 (peek-byte in (- byte 1))]
         ; img descriptor marker (should be 0x2C)
         [id-0 (peek-byte in byte)]
         ; packed fields in img descriptor
         [id-9 (byte->bits (peek-byte in (+ byte 9)))]
         ; is there a local color table?
         [lct? (equal? (first id-9) 1)]
         ; size of local color table
         [lct-size (if lct?
                       (expt 2 (+ 1 
                                  (* 4 (sixth id-9)) 
                                  (* 2 (seventh id-9))
                                  (eighth id-9)))
                       0)]
         ; termination byte of img descriptor header
         [id-term (peek-byte in (+ byte 9 lct-size))])
    (and
     (equal? id-m1 0)
     (equal? id-0 44)
     (equal? id-term 0))))

(define (extn? in byte)
  (let* (; img descriptor marker (should be 0x21)
         [id-0 (peek-byte in byte)]
         ; 2nd byte labels extension type
         [id-1 (peek-byte in (+ byte 1))]
         ; 3rd byte is often block size
         [id-2 (peek-byte in (+ byte 2))])
    (if [equal? id-0 33]
        ; extension types:
        (cond [(and
                (equal? id-1 255)
                (equal? id-2 11))
               #t] ; application
              [(equal? id-1 254)
               #t] ; comment
              [(and
                (equal? id-1 249)
                (equal? id-2 4))
               #t] ; graphic control
              [(and
                (equal? id-1 1)
                (equal? id-1 12))
               #t] ; plain text
              [else #f])
        #f)))

(define (img-size in byte)
  (let* (; packed fields in img descriptor
         [id-9 (byte->bits (peek-byte in (+ byte 9)))]
         ; size of local color table
         [lct-size (if (equal? (first id-9) 1)
                       (expt 2 (+ 1 
                                  (* 4 (sixth id-9)) 
                                  (* 2 (seventh id-9))
                                  (eighth id-9)))
                       0)]
         [data-first (+ byte 9 lct-size 1)])
    ; loop through data until not data
    (define (iter in b)
      (let ([val (peek-byte in b)])
        (cond [(equal? val eof) (error "Missed:" b)]
              [(or
                (trailer? in b)
                (img? in b)
                (extn? in b))
               (- b byte)]
              [else 
               (iter in (+ b val 1))])))
    (iter in data-first))) 

; ahh
(define-syntax-rule
  (gif: function filename args ...)
  (call-with-input-file filename
    (lambda (in) (function in args ...))))

(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")