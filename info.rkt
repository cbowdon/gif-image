#lang setup/infotab

(define name "gif-image")
(define scribblings '(("doc/manual.scrbl" ())))

(define blurb '("Library to extract still images from gifs."))
(define primary-file "main.rkt")

(define required-core-version "6.0")

(define deps '("base" "scribble-lib"))
(define build-deps '("racket-doc"))
