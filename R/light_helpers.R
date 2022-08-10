# `cheap_darken()` is a function copied from {isocubes}
# Originally MIT License: (c) 2022 mikefc@coolbutuseless.com

#' @param amount Fraction to darken by
#' @importFrom grDevices rgb col2rgb
#' @export
#' @examples
#'   cheap_darken("blue", 0.5)
#'   darken_face("top", "blue")
#' @rdname light_helpers
cheap_darken <- function(col, amount) {
  mat <- col2rgb(col, alpha = TRUE)
  mat[1:3, ] <- mat[1:3, ] * (1 - amount)
  rgb(mat[1, ], mat[2, ], mat[3, ], mat[4, ], maxColorValue = 255)
}

#' 'light' effect helper functions
#'
#' Helper functions to generate a \dQuote{light} effect
#' for [oblicubesGrob()], [grid.oblicubes()], [oblicuboidsGrob()], and [grid.oblicuboids()].
#' `darken_face()` is the default `light` argument for [oblicubesGrob()], [grid.oblicubes()],
#'  [oblicuboidsGrob()], and [grid.oblicuboids()].
#' `cheap_darken()` is the default darkening function used by `darken_face()`.
#'
#' The `light` argument of [oblicubesGrob()], [grid.oblicubes()], [oblicuboidsGrob()], and [grid.oblicuboids()]
#' needs a function that that takes two arguments: the first is `face` one of its five faces:
#' "top", "west", "east", "south", or "north" and the second is `col` the cube/cuboid's fill color
#'
#' @param col Color to darken
#' @param face Cube/cuboid face to color.  One of "top", "west", "east", "south", or "north".
#' @param top Amount to darken the "top" face.
#' @param west Amount to darken the "west" face.
#' @param east Amount to darken the "east" face.
#' @param south Amount to darken the "south" face.
#' @param north Amount to darken the "north" face.
#' @param darken_fn Function to darken with.  Should take two arguments:
#'                  the first should be the colour and the second should be numeric amount to darken by.
#'                  Default will be to use [cheap_darken()].
#'                  `colorspace::darken()` is a slower, \dQuote{better} alternative.
#' @rdname light_helpers
#' @return Vector of darkened colors.
#' @export
darken_face <- function(face, col,
                        top = 0.0,
                        west = 0.2,
                        east = 0.2,
                        south = 0.4,
                        north = 0.4,
                        darken_fn = cheap_darken) {
    switch(face,
           top = darken_fn(col, top),
           west = darken_fn(col, west),
           east = darken_fn(col, east),
           south = darken_fn(col, south),
           north = darken_fn(col, north),
           stop(paste("Unrecognized face", face)))
}

compute_fill <- function(fill, faces, light) {
    stopifnot(isTRUE(light) || isFALSE(light) || is.function(light))
    if (isFALSE(light)) {
        rep(fill, each = length(faces))
    } else {
        if (isTRUE(light))
            light <- darken_face
        fills <- lapply(faces, function(face) light(face, fill))
        do.call(splice1, fills)
    }
}
