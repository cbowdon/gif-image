#lang racket

(require rackunit
         "gif-split.rkt")

(define cwd (string->path "/Users/chris/Projects/gif-image/"))

(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")

(test-case
 "gif? recognises gifs"
 (for-each
  (lambda (x) (check-equal? (gif? (build-path "images" x)) #t))
  (filter (lambda (x) (regexp-match "gif" x)) (directory-list "images")))
 (check-equal? (gif? (build-path cwd "images/kif.png")) #f))

(test-case
 "gif: version works"
 (check-equal? (gif: version sunflower) "87a")
 (check-equal? (gif: version sample) "89a")
 (check-equal? (gif: version earth) "89a"))

(test-case
 "gif: logical-size correct"
 (check-equal? (gif: logical-size earth) (cons 400 400))
 (check-equal? (gif: logical-size sunflower) (cons 250 297))
 (check-equal? (gif: logical-size sample) (cons 3 3)))

(test-case
 "Can accurately find gif: global-color-table-size"
 (check-equal? (gif: gct-size sunflower) 256)
 (check-equal? (gif: gct-size earth) 256)
 (check-equal? (gif: gct-size sample) 4))

(test-case
 "gif: trailer? recognises EOF"
 (check-equal? (gif: trailer? sunflower (- (file-size sunflower) 1)) #t)
 (check-equal? (gif: trailer? sample (- (file-size sample) 1)) #t)
 (check-equal? (gif: trailer? earth (- (file-size earth) 1)) #t)
 (check-equal? (gif: trailer? earth 4) #f))

(test-case
 "gif: gce? recognises graphic control extension"
 (check-equal? (gif: gce? earth 800) #t))

(test-case
 "gif: gce returns a complete graphic control extension"
 (let* ([the-gce (gif: gce earth 800)]
        [gce-len (bytes-length the-gce)]
        [head-byte (bytes-ref the-gce 0)]
        [second-byte (bytes-ref the-gce 1)]
        [term-byte (bytes-ref the-gce (- gce-len 1))])
   (check-equal? gce-len 7)
   (check-equal? head-byte 33)
   (check-equal? second-byte 249)
   (check-equal? term-byte 0)))

(test-case
 "gif: img? recognises an image-descriptor"
 (check-equal? (gif: img? earth 808) #t)
 (check-equal? (gif: img? earth 850) #f)
 (check-equal? (gif: img? earth 37571) #t)
 (check-equal? (gif: img? earth 1391491) #t))

(test-case
 "gif: img-size returns the number of bytes of an image (one frame)"
 (let ([actual-size-1 (- 37563 808)]
       [calcd-size-1 (gif: img-size earth 808)]
       [actual-size-2 (- 73553 37571)]
       [calcd-size-2 (gif: img-size earth 37571)])
   ; note that subsequent frames may be diff size
   (check-equal? (gif: img? earth 808) #t)
   (check-equal? calcd-size-1 actual-size-1)
   (check-equal? (gif: img? earth 37571) #t)
   (check-equal? calcd-size-2 actual-size-2)))

(test-case
 "gif: img returns a complete image descriptor"
 (let* ([actual-size (- 37563 808)]
        [the-img (gif: img earth 808)]
        [img-len (bytes-length the-img)]
        [head-byte (bytes-ref the-img 0)]
        [term-byte (bytes-ref the-img (- img-len 1))])
   (check-equal? img-len actual-size)
   (check-equal? head-byte 44)
   (check-equal? term-byte 0)))

(test-case
 "gif: frames returns 44 images from earth"
 (check-equal? (length (gif: frames earth)) 44)
 (check-equal? (length (gif: frames sample)) 1)
 (check-equal? (length (gif: frames sunflower)) 1))

(require profile)
(profile-thunk (lambda () (gif: frames earth)))





