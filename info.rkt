#lang info

(define collection "glu-tessellate")

(define scribblings '(("glu-tessellate.scrbl")))

(define deps '("base"))

(define build-deps '("draw-doc"
                     ["gui-lib" #:version "1.8"]
                     "racket-doc"
                     "scribble-lib"))
