#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc)

(provide paths->triangles
         paths->edges)

(define glu-lib
  (case (system-type)
    [(windows) (ffi-lib "glu32")]
    [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU")]
    [else (ffi-lib "libGLU" '("1" ""))]))

(define-ffi-definer define-glu glu-lib)
  
(define _GLUtessellator-pointer (_cpointer 'GLUtessellator))
(define _GLdouble _double)
(define _GLenum _int)
(define _GLdouble3 (_array _GLdouble 3))
(define _GLdouble3-pointer _pointer)

(define GLU_TESS_BEGIN   100100)
(define GLU_TESS_END     100102)
(define GLU_TESS_VERTEX  100101)
(define GLU_TESS_COMBINE 100105)
(define GLU_TESS_ERROR   100103)

(define GLU_TESS_BOUNDARY_ONLY  100141)

(define GL_LINE_LOOP        #x0002)
(define GL_TRIANGLES        #x0004)
(define GL_TRIANGLE_STRIP   #x0005)
(define GL_TRIANGLE_FAN     #x0006)

(define-glu gluDeleteTess (_fun _GLUtessellator-pointer -> _void)
  #:wrap (deallocator))
(define-glu gluNewTess (_fun -> _GLUtessellator-pointer)
  #:wrap (allocator gluDeleteTess))

(define-glu gluTessBeginPolygon (_fun _GLUtessellator-pointer _pointer -> _void))
(define-glu gluTessBeginContour (_fun _GLUtessellator-pointer -> _void))
(define-glu gluTessVertex (_fun _GLUtessellator-pointer (p : _GLdouble3) (_pointer = (array-ptr p)) -> _void))
(define-glu gluTessEndContour (_fun _GLUtessellator-pointer -> _void))
(define-glu gluTessEndPolygon (_fun _GLUtessellator-pointer -> _void))

(define-glu gluTessCallback-BEGIN (_fun _GLUtessellator-pointer (_GLenum = GLU_TESS_BEGIN)
                                        (_fun #:atomic? #t _GLenum -> _void) -> _void)
  #:c-id gluTessCallback)
(define-glu gluTessCallback-END (_fun _GLUtessellator-pointer (_GLenum = GLU_TESS_END)
                                        (_fun #:atomic? #t -> _void) -> _void)
  #:c-id gluTessCallback)
(define-glu gluTessCallback-VERTEX (_fun _GLUtessellator-pointer (_GLenum = GLU_TESS_VERTEX)
                                         (_fun #:atomic? #t _GLdouble3 -> _void) -> _void)
  #:c-id gluTessCallback)
(define-glu gluTessCallback-COMBINE (_fun _GLUtessellator-pointer (_GLenum = GLU_TESS_COMBINE)
                                          (_fun #:atomic? #t _GLdouble3 _pointer _pointer _pointer -> _void) -> _void)
  #:c-id gluTessCallback)
(define-glu gluTessCallback-ERROR (_fun _GLUtessellator-pointer (_GLenum = GLU_TESS_ERROR)
                                        (_fun #:atomic? #t _GLenum -> _void) -> _void)
  #:c-id gluTessCallback)

(define-glu gluTessProperty (_fun _GLUtessellator-pointer _GLenum _GLdouble -> _void))
(define-glu gluGetTessProperty (_fun _GLUtessellator-pointer _GLenum (v : (_ptr o _GLdouble)) -> _void -> v))
(define-glu gluTessNormal (_fun _GLUtessellator-pointer _GLdouble _GLdouble _GLdouble -> _void))

(define-glu gluBeginPolygon (_fun _GLUtessellator-pointer -> _void))
(define-glu gluEndPolygon (_fun _GLUtessellator-pointer -> _void))

(define (paths->tessellation closed-paths
                             #:expected-scale scale
                             #:triangles? triangles?)

  (define ->i exact->inexact)

  (define (pt x y z)
    (define a (ptr-ref (malloc _GLdouble3 'atomic-interior) _GLdouble3))
    (array-set! a 0 x)
    (array-set! a 1 y)
    (array-set! a 2 z)
    (hash-set! keeps a #t)
    a)
  
  (define (unpt p)
    (cons (array-ref p 0)
          (array-ref p 1)))

  (define keeps (make-hash))
  (define error-scale (expt (/ 0.5 scale) 2))

  (define t (gluNewTess))
  
  (define mode GL_TRIANGLES)
  (define tris null)
  (define sets null)

  (define (tess-start v)
    (set! mode v))
  (define (tess-end)
    (set! sets (cons (cons mode tris) sets))
    (set! tris null))
  (define (tess-vertex p)
    (set! tris (cons (unpt p) tris)))
  (define (tess-combine p data weight dest)
    (ptr-set! dest _pointer (array-ptr (pt (array-ref p 0)
                                           (array-ref p 1)
                                           (array-ref p 2)))))
  (define (tess-error v)
    (log-error "paths->triangles: tessellation error: ~a" v))

  (hash-set! keeps tess-start #t)
  (hash-set! keeps tess-end #t)
  (hash-set! keeps tess-vertex #t)
  (hash-set! keeps tess-combine #t)
  (hash-set! keeps tess-error #t)
  
  (gluTessCallback-BEGIN t tess-start)
  (gluTessCallback-END t tess-end)
  (gluTessCallback-VERTEX t tess-vertex)
  (gluTessCallback-COMBINE t tess-combine)
  (gluTessCallback-ERROR t tess-error)
  
  (gluTessProperty t GLU_TESS_BOUNDARY_ONLY (if triangles?
                                                0.0
                                                1.0))
  
  (gluTessBeginPolygon t #f)
  (for ([closed-path (in-list closed-paths)])
    (gluTessBeginContour t)
    (for/fold ([prev-x 0.0] [prev-y 0.0]) ([p (in-list closed-path)])
      (cond
       [(= 2 (vector-length p))
        (define x (->i (vector-ref p 0)))
        (define y (->i (vector-ref p 1)))
        (gluTessVertex t (pt x y 0.0))
        (values x y)]
       [else
        (define (bezier x1 y1 x2 y2 x3 y3 x4 y4)
          (define dx (- x4 x1))
          (define dy (- y4 y1))
          (define d2 (abs (- (* dy (- x2 x4)) (* dx (- y2 y4)))))
          (define d3 (abs (- (* dy (- x3 x4)) (* dx (- y3 y4)))))
          (define d (+ d2 d3))
          (cond
           [((* d d) . <= . (* error-scale (+ (* dx dx) (* dy dy))))
            ;; close enough to a line
            (gluTessVertex t (pt x4 y4 0.0))
            (values x4 y4)]
           [else
            ;; divide & conquer
            (define (s a b) (/ (+ a b) 2))
            (define x12   (s x1 x2))
            (define y12   (s y1 y2))
            (define x23   (s x2 x3))
            (define y23   (s y2 y3))
            (define x34   (s x3 x4))
            (define y34   (s y3 y4))
            (define x123  (s x12 x23))
            (define y123  (s y12 y23))
            (define x234  (s x23 x34))
            (define y234  (s y23 y34))
            (define x1234 (s x123 x234))
            (define y1234 (s y123 y234))
            (bezier x1 y1 x12 y12 x123 y123 x1234 y1234)
            (bezier x1234 y1234 x234 y234 x34 y34 x4 y4)]))
        (bezier prev-x prev-y
                (->i (vector-ref p 0)) (->i (vector-ref p 1))
                (->i (vector-ref p 2)) (->i (vector-ref p 3))
                (->i (vector-ref p 4)) (->i (vector-ref p 5)))]))
    (gluTessEndContour t))
  (gluTessEndPolygon t)
  
  (gluDeleteTess t)
  
  (unless (positive? (hash-count keeps))
    (error "the reference that was to prevent GCing somehow went wrong"))
  
  (apply
   append
   (for/list ([s (in-list sets)])
     (define mode (car s))
     (cond
      [(= mode GL_LINE_LOOP)
       (define all-l (reverse (cdr s)))
       (let loop ([l all-l])
         (cond
          [(null? (cdr l))
           (list (vector (car l) (car all-l)))]
          [else
           (cons (vector (car l) (cadr l))
                 (loop (cdr l)))]))]
      [(= mode GL_TRIANGLES)
       (let loop ([l (reverse (cdr s))])
         (cond
          [(null? l) null]
          [else (cons (vector (car l) (cadr l) (caddr l))
                      (loop (cdddr l)))]))]
      [(= mode GL_TRIANGLE_STRIP)
       (let loop ([l (reverse (cdr s))] [rev? #f])
         (cond
          [(null? (cddr l)) null]
          [else
           (cons (if rev?
                     (vector (cadr l) (car l) (caddr l))
                     (vector (car l) (cadr l) (caddr l)))
                 (loop (cdr l) (not rev?)))]))]
      [(= mode GL_TRIANGLE_FAN)
       (define l (reverse (cdr s)))
       (define c (car l))
       (let loop ([l (cdr l)])
         (cond
          [(null? (cdr l)) null]
          [else (cons (vector c (car l) (cadr l))
                      (loop (cdr l)))]))]
      [else (error "unknown mode")]))))

(define (paths->triangles closed-paths
                          #:expected-scale [expected-scale 1.0])
  (paths->tessellation closed-paths
                       #:expected-scale expected-scale
                       #:triangles? #t))

(define (paths->edges closed-paths
                      #:expected-scale [expected-scale 1.0])
  (paths->tessellation closed-paths
                       #:expected-scale expected-scale
                       #:triangles? #f))
