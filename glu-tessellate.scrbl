#lang scribble/manual
@(require (for-label glu-tessellate
                     racket/base
                     racket/contract/base
                     racket/gui/base))

@title{Tessellation via GLU}

@defmodule[glu-tessellate]{The @racketmodname[glu-tessellate] module
uses the platform's GLU library to convert a set of closed paths to
either a set of triangles for the bounded shape's interior or a set of
line segments for the bounded shape's edges.}

@defproc[(paths->triangles [closed-paths (listof (listof vector?))]
                           [#:expected-scale expected-scale real? 1.0])
         (listof (vector/c (cons/c flonum? flonum?)
                           (cons/c flonum? flonum?)
                           (cons/c flonum? flonum?)))]{

Produces a set of triangles given a set of closed paths. The triangles
cover the space bounded by the combined paths.

Each closed path must start with a vector of two numbers, which
represents the path's starting and ending point. Subsequent elements
are vectors of either two or six numbers, where a vector of six
numbers represents a Bezier curve from the preceding point to the
point specified by the last two elements of the vector, where the
first four elements of the vector represent control points. (This
format is consistent with the result of @xmethod[dc-path% get-datum].)

The resulting triangles are each represented by a vector of three
points, where a point is represented by a pair of numbers.

Paths can include Bezier curves, which must be approximated by lines.
The @racket[expected-scale] argument adjusts the approximation based
on how much the resulting triangles are expected to be scaled relative
to a rendering unit.}

@defproc[(paths->edges [closed-paths (listof (listof vector?))]
                       [#:expected-scale expected-scale real? 1.0])
         (listof (vector/c (cons/c flonum? flonum?)
                           (cons/c flonum? flonum?)))]{

Like @racket[paths->triangles], but produces a set of line sequences
for the edge of the area bounded by the combined paths. For each edge,
the bounded area is to the left of the edge moveing from the starting
point to the ending point.}
