#lang racket

(provide byte->bits
         bits->byte
         bytes->short
         bytes->coord
         null-byte?)

(define (byte->bits byte)
  (define (iter bit value bits-list)
    (cond [(or (< value 0) (> value 255)) (error "Not a byte:" value)]
          [(equal? (length bits-list) 8) bits-list]
          [(equal? 0 value) (iter 0 0 (cons 0 bits-list))]
          [(< (- value bit) 0)
           (iter (/ bit 2) value (cons 0 bits-list))]
          [else 
           (iter (/ bit 2) (- value bit) (cons 1 bits-list))]))
  (iter 128 byte '()))

(define (bits->byte bits-list)
  (define (iter value bits block)
    (cond [(empty? bits) value]
          [else (iter (+ value (* (car bits) block)) (cdr bits) (* 2 block))]))
  (if [equal? (length bits-list) 8]
      (iter 0 bits-list 1)
      (error "Bits list wrong length:" bits-list)))

; extract an unsigned short
(define (bytes->short data [byte 0])
  (if [< (bytes-length data) (+ byte 2)]
      (error "bytes->short: input byte-string too short" data)
      (let ([x0 (bytes-ref data byte)]
            [x1 (bytes-ref data (+ byte 1))])
        (+ x0 (* x1 256)))))

; extract any pair of unsigned shorts
(define (bytes->coord data [byte 0])
  (if [< (bytes-length data) (+ byte 4)]
      (error "bytes->short: input byte-string too short" data)
      (cons
       (bytes->short data byte)
       (bytes->short data (+ byte 2)))))

(define (null-byte? data b)
  (equal? (bytes-ref data b) 0))
