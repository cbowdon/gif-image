#lang racket

(require rackunit
         "gif-split.rkt")

(define cwd (string->path "/Users/chris/Projects/gif-split/"))

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
 "Can accurately find global-color-table-size"
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
 "gif: img? recognises an image-descriptor"
 (check-equal? (gif: img? earth 808) #t)
 (check-equal? (gif: img? earth 850) #f))

(test-case
 "gif: img-size returns the bytes of an image"
 (let ([img-size (- 37563 808)])
   (check-equal? (gif: img-size earth 808) img-size)))


