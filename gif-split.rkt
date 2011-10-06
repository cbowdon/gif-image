#lang racket

(provide gif?
         version
         logical-size
         gct-size
         header
         trailer?
         gce?
         gce-size
         gce-time
         comment?
         comment-size
         plain-text?
         plain-text-size
         appn?
         appn-size
         img?
         img-size
         img-dimensions
         frames
         timings
         comments
         animated?
         gif-build
         images
         gif:)

(require "bits-and-bytes.rkt")

(define (gif? data)
    (and
     (equal? (subbytes data 0 3) #"GIF")
     (equal? (bytes-ref data (- (bytes-length data) 1)) 59)))

(define (version data)
  (bytes->string/utf-8 (subbytes data 3 (+ 3 3))))

(define (logical-size data)
  (extract-coord data 6))

; extract an unsigned short
(define (extract-short data byte)
  (let ([x0 (bytes-ref data byte)]
        [x1 (bytes-ref data (+ byte 1))])
    (+ x0 (* x1 256))))

; extract any pair of unsigned shorts
(define (extract-coord data byte)
  (cons
   (extract-short data byte)
   (extract-short data (+ byte 2))))

(define (gct-size data)
  (let* ([packed-field (byte->bits (bytes-ref data 10))]
         [flag (first packed-field)]
         [i (+ 1
               (* flag ; could be b&w
                  (+ 
                   (* 4 (sixth packed-field)) 
                   (* 2 (seventh packed-field))
                   (eighth packed-field))))])
    (expt 2 i)))

(define (header data)
  (subbytes data 0 (+ 12 (* 3 (gct-size data)) 1)))

(define (trailer? data byte) 
  (and
   (equal? byte (- (bytes-length data) 1))
   (equal? (bytes-ref data byte) 59)))

(define (extn? data byte)
  (or
   (gce? data byte)
   (comment? data byte)
   (appn? data byte)
   (plain-text? data byte)))

(define (null-byte? data b)
  (equal? (bytes-ref data b) 0))

; traverse data sub-blocks
(define (subblocks-size data b)
  (let ([len (bytes-length data)])
    (cond [(> b len) (error "subblocks-size: exceeded EOF")]
          [(or
            ; more efficiency here would be
            ; a let with the bytes-refs
            ; and pass these to the preds
            (trailer? data b)
            (img? data b)
            (extn? data b))
           b]
          ; termination byte?
          [(null-byte? data b) (+ b 1)]
          [else 
           ; jumps over subblocks
           (subblocks-size data (+ b (bytes-ref data b) 1))])))

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

(define (gce-size data byte) 8)

(define (gce-time data byte)
  (/ (extract-short data (+ byte 4)) 100))

; deprecated
(define (gce data byte)
  (if [gce? data byte]
      (subbytes data byte (+ byte 7))
      (error "Not a graphic control extension")))

(define (comment? data byte)
  (if [< (+ byte 2) (bytes-length data)]
      (let (; img descriptor marker (should be 0x21)
            [id-0 (bytes-ref data byte)]
            ; 2nd byte labels extension type
            [id-1 (bytes-ref data (+ byte 1))])
        (and 
         (equal? id-0 33)
         (equal? id-1 254)))
      #f))

(define (comment-size data byte)
  (let* (; data sub-blocks start at 3rd byte
         [data-first (+ byte 2)]
         [sb-size (subblocks-size data data-first)])
    (- sb-size byte)))

; untested!
(define (plain-text? data byte)
  (if [< (+ byte 16) (bytes-length data)]
      (let (; first byte should be extn marker
            [id-0 (bytes-ref data byte)]
            ; plain-text extension marker
            [id-1 (bytes-ref data (+ byte 1))]
            ; size marker is always 12
            [id-2 (bytes-ref data (+ byte 2))])
        (and
         (equal? id-0 33)
         (equal? id-1 1)
         (equal? id-2 12)))
      #f))

; untested!
(define (plain-text-size data byte)
  (let* (; data sub-blocks start at 16th byte
         [data-first (+ byte 15)]
         [sb-size (subblocks-size data data-first)])
    (- sb-size byte)))

(define (appn? data byte)
  (if [< (+ byte 16) (bytes-length data)]
      (let (; first byte should be extn marker
            [id-0 (bytes-ref data byte)]
            ; application extension marker
            [id-1 (bytes-ref data (+ byte 1))]
            ; size marker is always 11
            [id-2 (bytes-ref data (+ byte 2))])
        (and
         (equal? id-0 33)
         (equal? id-1 255)
         (equal? id-2 11)))
      #f))

(define (appn-size data byte)
  (let* (; data sub-blocks start at 15th byte
         [data-first (+ byte 14)]
         [sb-size (subblocks-size data data-first)])
    (- sb-size byte)))

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
        (and
         (equal? id-0 44)
         (<= (car idims) (car gdims))
         (<= (cdr idims) (cdr gdims))
         (<= (car corner) (car gdims))
         (<= (cdr corner) (cdr gdims))))
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
         ; LZW-min-size 
         [LZW-min (+ byte 9 lct-size 1)]
         ; first byte of data subblocks
         [data-first (+ byte 9 lct-size 2)]
         ; length of file
         [len (bytes-length data)])
    ; loop through data until not data
    (- (subblocks-size data data-first) byte))) 

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

; return all instances of a particular kind of subblock
(define (subblocks data pred? size)
  (define (sbs-iter byte sbs)
    (cond [(trailer? data byte) sbs]
          [(pred? data byte)
           (let ([s (size data byte)])
             (sbs-iter (+ byte s) (stream-cons (subbytes data byte (+ byte s)) sbs)))]
          [else (sbs-iter (+ byte 1) sbs)]))
  (sbs-iter 0 '()))

; return frames
(define (frames data)
  (subblocks data img? img-size))
;(subblocks data gce? (lambda (data byte) 7)))

; is animated?
(define (animated? data)
  (> (stream-length (images data)) 1))

; return times in every gce
(define (timings data)
  (stream-map (lambda (x) (gce-time x 0)) (subblocks data gce? gce-size)))  

; return all the comments
(define (comments data)
  (stream-map 
   ; assumes no comment > 256 bytes
   (lambda (x) (bytes->string/utf-8 (subbytes x 3 (- (bytes-length x) 1)))) 
   (subblocks data comment? comment-size)))

; make a gif
(define (gif-build filename-out hdr blocks)
  (call-with-output-file filename-out
    (lambda (out) 
      (if [or (stream? blocks) (list? blocks)]
          (write-bytes (bytes-append (apply bytes-append hdr (stream->list blocks)) #";") out)
          (write-bytes (bytes-append hdr blocks #";") out)))))

; valid images of all
(define (images data)
  (stream-map
   (lambda (x) (bytes-append (header data) x #";"))
   (subblocks data img? img-size)))

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
(define earth-small "images/200px-Rotating_earth_(large).gif")
(define newton "images/Newtons_cradle_animation_book_2.gif")
