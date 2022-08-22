# `cheap_darken()` is a function copied from {isocubes}
# Originally MIT License: (c) 2022 mikefc@coolbutuseless.com

#' @param amount Fraction to darken by
#' @importFrom grDevices rgb col2rgb
#' @export
#' @examples
#' demo_light <- function(light = darken_face, ...) {
#'   df <- data.frame(x=1, y=1, z=1)
#'   grid::grid.newpage()
#'   grid.oblicubes(df, ..., light=light, angle=45, lwd=4,
#'                  vp = grid::viewport(0.25, 0.25, 0.5, 0.5))
#'   grid.oblicubes(df, ..., light=light, angle=135, lwd=4,
#'                  vp = grid::viewport(0.75, 0.25, 0.5, 0.5))
#'   grid.oblicubes(df, ..., light=light, angle=-45, lwd=4,
#'                  vp = grid::viewport(0.25, 0.75, 0.5, 0.5))
#'   grid.oblicubes(df, ..., light=light, angle=-135, lwd=4,
#'                  vp = grid::viewport(0.75, 0.75, 0.5, 0.5))
#' }
#' demo_light()
#' demo_light(fill = "gold")
#' demo_light(light = function(face, col)
#'              darken_face(face, col, top = 0.3,
#'                          west = 0.6, east = 0.6,
#'                          south = 0.0, north = 0.0)
#' )
#' demo_light(light = function(face, col) {
#'              n <- length(col)
#'              switch(face,
#'                top = rep_len("grey90", n),
#'                west = rep_len("red", n),
#'                east = rep_len("green", n),
#'                south = rep_len("blue", n),
#'                north = rep_len("yellow", n))
#'            })
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
#' The `light` argument of [oblicubesGrob()], [grid.oblicubes()], [geom_oblicubes()],
#' [oblicuboidsGrob()], [grid.oblicuboids()], and [geom_oblicuboids()]
#' needs a function that that takes two arguments: the first is `face` one of its five faces:
#' "top", "west", "east", "south", or "north" and the second is `col` the cube/cuboid's fill color
#'
#' @param face Cube/cuboid face to color.  One of "top", "west", "east", "south", or "north".
#' @param col Vector of colors to darken
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
