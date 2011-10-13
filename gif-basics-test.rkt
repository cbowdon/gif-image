#lang racket

(require rackunit
         rackunit/text-ui
         "gif-basics.rkt")

(define cwd (string->path "/Users/chris/Projects/gif-image/"))

(define kif "images/kif.png")
(define sunflower "images/Sunflower_as_gif_websafe.gif")
(define sample "images/sample.gif")
(define earth "images/Rotating_earth_(large).gif")
(define earth-small "images/200px-Rotating_earth_(large).gif")
(define newton "images/Newtons_cradle_animation_book_2.gif")
(define shoe "images/200px-BananaShoeShine.gif")
(define honey "images/honeycakecoffee.gif")
(define my "images/my.gif")

(define-syntax-rule
  (gif: function filename args ...)
  (call-with-input-file filename
    (lambda (in) 
      (let ([data (read-bytes (file-size filename) in)])        
        (function data args ...)))))

(define/provide-test-suite
  basics-tests
  
  (test-case
   "gif: header? works"
   (check-equal? (gif: header? earth) #t)
   (check-equal? (gif: header? sunflower) #t)
   (check-equal? (gif: header? kif) #f))
  
  (test-case
   "gif: header-size works"
   (check-equal? (gif: header-size earth) (+ 13 (* 3 256)))
   (check-equal? (gif: header-size sunflower) (+ 13 (* 3 256)))
   (check-equal? (gif: header-size sample) 19)
   ; followed by extn?
   (let ([hs (gif: header-size earth)])
     (check-equal? (gif: appn? earth hs) #t)))
  
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
         [calcd-size-2 (gif: img-size earth 37571)]
         [actual-size-3 (- 114695 81475)]
         [calcd-size-3 (gif: img-size honey 81475)])
     ; note that subsequent frames may be diff size
     (check-equal? (gif: img? earth 808) #t)
     (check-equal? calcd-size-1 actual-size-1)
     (check-equal? (gif: img? earth 37571) #t)
     (check-equal? calcd-size-2 actual-size-2)
     (check-equal? (gif: img? honey 81475) #t)
     (check-equal? calcd-size-3 actual-size-3)))
   
  (test-case
   "gif: gce? recognises graphic control extensions"
   (check-equal? (gif: gce? earth 800) #t)
   (check-equal? (gif: gce? earth 801) #f)
   (check-equal? (gif: gce? honey 81467) #t))
  
  (test-case
   "gif: gce-size is right"
   (check-equal? (gif: gce-size earth 800) 8))
  
  (test-case
   "gif: comment? recognises a comment"
   (check-equal? (gif: comment? sample 19) #t)
   (check-equal? (gif: comment? earth 19) #f))
  
  (test-case
   "gif: comment-size correctly gets length of comment"
   (check-equal? (gif: comment-size sample 19) 16))
  
  (test-case
   "gif: appn? correctly recognises an application extension"
   (check-equal? (gif: appn? earth 781) #t)
   (check-equal? (gif: appn? earth 780) #f)
   (check-equal? (gif: appn? sample 2) #f))
  
  (test-case
   "gif: appn-size returns size of application extension"
   (check-equal? (gif: appn-size earth 781) (- 800 781)))
  
  (test-case
   "gif: trailer? recognises EOF"
   (check-equal? (gif: trailer? sunflower (- (file-size sunflower) 1)) #t)
   (check-equal? (gif: trailer? sample (- (file-size sample) 1)) #t)
   (check-equal? (gif: trailer? earth (- (file-size earth) 1)) #t)
   (check-equal? (gif: trailer? earth 4) #f))
  
  (test-case
   "gif: find-next-n works"
   (check-equal? (gif: find-next-n earth-small 0 gce? gce-size 1) 781)
   (check-equal? (gif: find-next-n earth-small 0 img? img-size 1) 890)
   (check-equal? (gif: find-next-n earth-small 0 gce? gce-size 2) 14164)
   (check-equal? (gif: find-next-n newton 294137 img? img-size 1) 294145)
   (check-equal? (gif: find-next-n newton 294137 gce? gce-size 2) 298837)
   (check-equal? (gif: find-next-n newton 298837 img? img-size 1) 298845)
   (check-equal? (gif: find-next-n newton 303954 img? img-size 1) 303962)))

; run the tests
(run-tests basics-tests)