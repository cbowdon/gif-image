#lang scribble/manual

@(require (for-label racket "../gif-decompose.rkt"))

@title{GIF Decompose}

@racket{gif-decompose.rkt} is a module to extract still images from GIF animations. It is a
complement to @racket{file/gif}, which enables composing GIF animations from bitmap images.

@section[#:tag "images"]{Extracting GIF images}

@defproc[(gif? [v any/c])(boolean?)]{
 Returns @racket[#t] if @racket[v] is a path to a valid GIF file
 or a byte-string containing a valid GIF file.
}

@defproc[(gif-images [g gif?])(stream?)]{
 Returns a finite stream of still images from @racket[g].
 Each element in the stream is a byte-string containing a valid GIF file.
}

@defproc[(gif-write-images [g gif?] [name string?]) (boolean?)]{
 Like @racket[gif-images] but writes each element of the stream to a file. Files are named according
 to @racket[name-X.gif] where @racket[X] is the number of the image in the original animation.
 The return value is @racket[#t] if all images were written successfully.
}

@section[#:tag "utilities"]{GIF utilities}

Additional helper procedures are provided for extracting metadata and debugging. 

@defproc[(gif-dimensions [g gif?])(pair?)]{
 Returns a pair containing the x and y dimensions (in pixels) of the image.
}

@defproc[(gif-animated? [g gif?])(boolean?)]{
 Returns @racket[#t] if @racket[g] is animated.
}

@defproc[(gif-timings [g gif?])(listof rational?)]{
 Returns a list of delay times (in seconds) for @racket[g].
}

@defproc[(gif-comments [g gif?])(stream?)]{
 Returns a stream of the comments in @racket[g], where each comment is a byte-string.
 Encoding the comments is left as an exercise for the reader.
}

@defproc[(gif-print-blocks [g gif?])(boolean?)]{
 Prints the structure of @racket[g] to @racket[current-output-port], in terms of byte number and
 block type (e.g. header, graphic control, image, comment...). Returns @racket[#t] after printing
 the entire GIF file.
}

