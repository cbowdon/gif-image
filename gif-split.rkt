#lang racket

(provide gif?
         version
         logical-size
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

(define (version data)
  (bytes->string/utf-8 (subbytes data 3 (+ 3 3))))

(define (logical-size data)
  (extract-coord data 6))

; extract any pair of unsigned shorts
(define (extract-coord data byte)
  (let ([x0 (bytes-ref data byte)]
        [x1 (bytes-ref data (+ byte 1))]
        [y0 (bytes-ref data (+ byte 2))]
        [y1 (bytes-ref data (+ byte 3))])
    (cons
     (+ x0 (* x1 256))
     (+ y0 (* y1 256)))))

(define (gct-size data)
  (let* ([packed-field (byte->bits (bytes-ref data 10))]
         [i (+ 1 
               (* 4 (sixth packed-field)) 
               (* 2 (seventh packed-field))
               (eighth packed-field))])
    (expt 2 i)))

(define (trailer? data byte) 
  (and
   (equal? byte (- (bytes-length data) 1))
   (equal? (bytes-ref data byte) 59)))

(define (extn? data byte)
  (if [< (+ byte 2) (bytes-length data)]
      (let* (; img descriptor marker (should be 0x21)
             [id-0 (bytes-ref data byte)]
             ; 2nd byte labels extension type
             [id-1 (bytes-ref data (+ byte 1))]
             ; 3rd byte is often block size
             [id-2 (bytes-ref data (+ byte 2))])
        (if [equal? id-0 33]
            ; extension types:
            (cond [(equal? id-1 254) #t] ; comment
                  [(gce? data byte) #t] ; graphic control
                  [(and
                    (equal? id-1 255)
                    (equal? id-2 11))
                   #t] ; application
                  [(and
                    (equal? id-1 1)
                    (equal? id-1 12))
                   #t] ; plain text
                  [else #f])
            #f))
      #f))

(define (gce? data byte)
  (if [< (+ byte 6) (bytes-length data)]
      (let (; first byte should be extn marker
            [id-0 (bytes-ref data byte)]
            ; graphic control extension marker
            [id-1 (bytes-ref data (+ byte 1))]
            ; size marker is always 4
            [id-2 (bytes-ref data (+ byte 2))]
            ; termination byte
            [id-6 (bytes-ref data (+ byte 6))])
        (and
         (equal? id-0 33)
         (equal? id-1 249)
         (equal? id-2 4)
         (equal? id-6 0)))
      #f))

; deprecated
(define (gce data byte)
  (if [gce? data byte]
      (subbytes data byte (+ byte 7))
      (error "Not a graphic control extension")))

; looking up bytes takes time
; could add gdims as an argument
; to speed this procedure up
(define (img? data byte)
  (if [< (+ byte 9) (bytes-length data)]
      (let* (; img descriptor marker (should be 0x2C)
             [id-0 (bytes-ref data byte)]
             ; gif dimensions
             [gdims (logical-size data)]
             ; image corner
             [corner (img-corner data byte)]
             ; image dimensions 
             [idims (img-dimensions data byte)])
        ; packed fields in img descriptor
        ;[id-9 (byte->bits (bytes-ref data (+ byte 9)))]
        ; is there a local color table?
        ;[lct? (equal? (first id-9) 1)]
        ; size of local color table
        ;[lct-size (if lct?
        ;             (expt 2 (+ 1 
        ;                       (* 4 (sixth id-9)) 
        ;                      (* 2 (seventh id-9))
        ;                     (eighth id-9)))
        ;         0)])
        ; termination byte of img descriptor header
        ;[id-term (bytes-ref data (+ byte 9 lct-size))])
        (and
         (equal? id-0 44)
         (<= (car idims) (car gdims))
         (<= (cdr idims) (cdr gdims))
         (<= (car corner) (car gdims))
         (<= (cdr corner) (cdr gdims))
         ;(equal? id-term 0)
         ))
      #f))

(define (img-size data byte)
  (let* (; packed fields in img descriptor
         [id-9 (byte->bits (bytes-ref data (+ byte 9)))]
         ; size of local color table
         [lct-size (if (equal? (first id-9) 1)
                       (expt 2 (+ 1 
                                  (* 4 (sixth id-9)) 
                                  (* 2 (seventh id-9))
                                  (eighth id-9)))
                       0)]
         [data-first (+ byte 9 lct-size 1)])
    ; loop through data until not data
    (define (img-size-iter b)
      (cond [(or
              (trailer? data b)
              (img? data b)
              (extn? data b))
             (- b byte)]
            [else 
             (img-size-iter (+ b 1))]))
    (img-size-iter data-first))) 

; data on image descriptors
(define (img-dimensions data byte)
  (extract-coord data (+ byte 5)))
(define (img-corner data byte)
  (extract-coord data (+ byte 1)))

; returns bytes of the image
; deprecated
(define (img data byte)
  (if [img? data byte]
      (subbytes data byte (+ byte (img-size data byte)))
      (error "Not an image descriptor" byte)))

; return frames
(define (frames data)
  (subblocks data img? img-size))

; return all instances of a particular kind of subblock
(define (subblocks data pred? size)
  (define (sbs-iter byte sbs)
    (cond [(trailer? data byte) sbs]
          [(pred? data byte)
           (let ([s (size data byte)])
             (sbs-iter (+ byte s) (cons (subbytes data byte (+ byte s)) sbs)))]
          [else (sbs-iter (+ byte 1) sbs)]))
  (sbs-iter 0 '()))


; ahh
(define-syntax-rule
  (gif: function filename args ...)
  (call-with-input-file filename
    (lambda (in) 
      (let ([data (read-bytes (file-size filename) in)])        
        (function data args ...)))))

(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")