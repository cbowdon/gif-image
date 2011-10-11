#lang racket

(provide gif?
         gif-dimensions
         gif-images
         gif-write-images
         gif-animated?
         gif-timings
         gif-comments
         gif-print-blocks)

(require "gif-basics.rkt"
         "bits-and-bytes.rkt")

(define (read-gif x)
  (cond [(bytes? x) x]
        [(or (path? x) (string? x))
         (call-with-input-file x (lambda (in) (read-bytes (file-size x) in)))]
        [else (error "read-gif: invalid input:" x)]))

(define (gif? x)
  (let ([data (read-gif x)])
    (and
     (header? data)
     (has-n-subblocks? data img? img-size 1)
     (trailer? data (- (bytes-length data) 1)))))

(define (gif-dimensions x)  
  (bytes->coord (read-gif x) 6))

(define (gif-images x)
  (let* ([data (read-gif x)]
         [hdr (subbytes data 0 (header-size data))]
         [trl #";"])
    ; return all images
    ; with the GCE attached if available
    (define (gi-iter byte gis)
      (cond [(trailer? data byte) (stream-reverse gis)]            
            [(gce? data byte)
             (let* ([gs (gce-size data byte)]
                    [img-byte (find-next-n data byte img? img-size 1)]
                    [size (- (+ img-byte (img-size data img-byte)) byte)])
               (gi-iter (+ byte size) (stream-cons (subbytes data byte (+ byte size)) gis)))]
            [(img? data byte)
             (let ([size (img-size data byte)])
               (gi-iter (+ byte size) (stream-cons (subbytes data byte (+ byte size)) gis)))]
            [else (gi-iter (+ byte 1) gis)]))
    (stream-map (lambda (x) (bytes-append hdr x trl)) (gi-iter 0 empty-stream))))

(define (gif-write-images x outfile)
  (define (writer imgs count)
    (let ([name (path-replace-suffix outfile (string-append "-" (number->string count) ".gif"))])
      (cond [(stream-empty? imgs) #t]
            [else 
             (begin
               (call-with-output-file name
                 (lambda (out) (write-bytes (stream-first imgs) out)))
               (writer (stream-rest imgs) (+ count 1)))])))
  (writer (gif-images x) 0))


; debugging use only
(define (gif-copy x filename-out)
  (let* ([data (read-gif x)]
         [hdr (subbytes data 0 (header-size data))]
         [trl #";"])
    ; return all images
    ; with the GCE attached if available
    (define (gi-iter byte gis)
      (cond [(trailer? data byte) (stream-reverse gis)]
            [(gce? data byte)
             (let* ([gs (gce-size data byte)]
                    [img-byte (find-next-n data byte img? img-size 1)]
                    [size (- (+ img-byte (img-size data img-byte)) byte)])
               (gi-iter (+ byte size) (stream-cons (subbytes data byte (+ byte size)) gis)))]
            [(img? data byte)
             (let ([size (img-size data byte)])
               (gi-iter (+ byte size) (stream-cons (subbytes data byte (+ byte size)) gis)))]
            [else (gi-iter (+ byte 1) gis)]))
    (call-with-output-file filename-out
      (lambda (out)
        (write-bytes
         (bytes-append (apply bytes-append hdr (stream->list (gi-iter 0 empty-stream))) trl) 
         out)))))

; more than one image?
(define (gif-animated? x)
  (has-n-subblocks? (read-gif x) img? img-size 2))

; return times in every gce
(define (gif-timings x)
  (let ([data (read-gif x)])
    (stream-map 
     (lambda (x) (/ (bytes->short x 4) 100))
     (subblocks data gce? gce-size))))

; return all the comments
(define (gif-comments x)
  (let ([data (read-gif x)])
    (stream-map      
     (lambda (x) (subbytes x 3 (- (bytes-length x) 1))) 
     (subblocks data comment? comment-size))))

; for debugging
(define (gif-print-blocks x)
  (let ([data (read-gif x)])
    (define (loop b)
      (cond 
        [(trailer? data b)
         (printf "~a\t\ttrailer\n" b)]
        [(img? data b)
         (begin
           (printf "~a\t\timg\n" b)
           (loop (+ b (img-size data b))))]
        [(gce? data b)
         (begin
           (printf "~a\t\tgce\n" b)
           (loop (+ b (gce-size data b))))]
        [(appn? data b)
         (begin
           (printf "~a\t\tappn\n" b)
           (loop (+ b (appn-size data b))))]
        [(comment? data b)
         (begin
           (printf "~a\t\tcomment\n" b)
           (loop (+ b (comment-size data b))))]
        [(plain-text? data b)
         (begin
           (printf "~a\t\tplain-text\n" b)
           (loop (+ b (plain-text-size data b))))]
        [(header? data b)
         (begin
           (printf "~a\t\theader\n" b)
           (loop (+ b (header-size data))))]
        [else
         (begin
           (printf "~a\t\tunknown\t~a\n" b (bytes-ref data b))
           (loop (+ b 1)))])) 
    (loop 0)))











