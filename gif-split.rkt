#lang racket

(provide gif?
         version
         gct-size
         trailer?
         gce?
         gce
         img?
         img-size
         img-dimensions
         img
         frames
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

(define (extn? in byte)
  (let* (; img descriptor marker (should be 0x21)
         [id-0 (peek-byte in byte)]
         ; 2nd byte labels extension type
         [id-1 (peek-byte in (+ byte 1))]
         ; 3rd byte is often block size
         [id-2 (peek-byte in (+ byte 2))])
    (if [equal? id-0 33]
        ; extension types:
        (cond [(equal? id-1 254) #t] ; comment
              [(gce? in byte) #t] ; graphic control
              [(and
                (equal? id-1 255)
                (equal? id-2 11))
               #t] ; application
              [(and
                (equal? id-1 1)
                (equal? id-1 12))
               #t] ; plain text
              [else #f])
        #f)))

(define (gce? in byte)
  (let (; first byte should be extn marker
        [id-0 (peek-byte in byte)]
        ; graphic control extension marker
        [id-1 (peek-byte in (+ byte 1))]
        ; size marker is always 4
        [id-2 (peek-byte in (+ byte 2))]
        ; termination byte
        [id-6 (peek-byte in (+ byte 6))])
    (and
     (equal? id-0 33)
     (equal? id-1 249)
     (equal? id-2 4)
     (equal? id-6 0))))

(define (gce in byte)
  (if [gce? in byte]
      (peek-bytes 7 byte in)
      (error "Not a graphic control extension")))

(define (img? in byte)
  (let* (; img descriptor marker (should be 0x2C)
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
     (equal? id-0 44)
     (equal? id-term 0))))

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

; data on images
(define (img-dimensions in byte)
  (if [img? in byte]
      (extract-coord in (+ byte 5))
      (error "Not an image descriptor")))
(define (img-corner in byte)
  (if [img? in byte]
      (extract-coord in (+ byte 1))
      (error "Not an image descriptor")))

; extract any pair of unsigned shorts
(define (extract-coord in byte)
  (let ([x0 (peek-byte in byte)]
        [x1 (peek-byte in (+ byte 1))]
        [y0 (peek-byte in (+ byte 2))]
        [y1 (peek-byte in (+ byte 3))])
    (cons
     (+ x0 (* x1 256))
     (+ y0 (* y1 256)))))

; returns bytes of the image
(define (img in byte)
  (if [img? in byte]
      (peek-bytes (img-size in byte) byte in)
      (error "Not an image descriptor" byte)))

; return frames
(define (frames in)
 (subblocks in gce? (lambda (in b) 7) gce))
 ; (subblocks in img? img-size img))

; return all instances of a particular kind of subblock
(define (subblocks in pred? size get)
  (define (iter byte sbs)
    (cond [(trailer? in byte) sbs]
          [(pred? in byte) 
           (begin
             (printf "~a:\t~a\t~a\t~a\t" 
                     (+ (length sbs) 1) 
                     byte 
                     (size in byte) 
                     (extract-coord in (+ byte 5)))
             (printf "+8:img? ~a\n" (img? in (+ byte 8)))
             (iter (+ byte (size in byte)) (cons (get in byte) sbs)))]
          [else (iter (+ byte 1) sbs)]))
  (iter 0 '()))


; ahh
(define-syntax-rule
  (gif: function filename args ...)
  (call-with-input-file filename
    (lambda (in) (function in args ...))))

(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")