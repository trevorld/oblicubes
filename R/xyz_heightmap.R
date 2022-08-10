# Copies and derivatives of functions originally from {isocubes}
# Originally MIT License: (c) 2022 mikefc@coolbutuseless.com

if (getRversion() >= "2.15.1")  utils::globalVariables("z")

# #' Cheap version of darkening a colour. much cheaper than colorspace package
# #' @param fill vector of R colours
# #' @param amount fraction to darken by
# #' @noRd
# #' @importFrom grDevices rgb col2rgb
# cheap_darken <- function(fill, amount) {
#   mat <- col2rgb(fill, alpha = TRUE)
#   mat[1:3,] <- mat[1:3,] * (1 - amount)
#   rgb(mat[1,], mat[2,], mat[3,], mat[4,], maxColorValue = 255)
# }

# Compared to `coords_heightmap()` we don't try to remove hidden cubes
# By default "up" is 'z' coordinates (which are always positive) and we support more "ground" values

#' Calculate x,y,z coordinates from a height matrix
#'
#' @param mat integer matrix. The matrix will be interpreted as cubes (or cuboids) flat on the
#'        page, with the value in the matrix interpreted as the height above the page.
#' @param col matrix, vector, or (palette) function of colours.
#'        If a matrix it must be the same dimensions as the `mat` argument;
#'        each cube/cuboid corresponding to that x,y value will have that color.
#'        If a vector then if the max of `z` values is less than equal to the number
#'        of colors we will use the `z` integers
#'        as indices else we will use [base::cut()] to assign z values to colors.
#'        If a function we will call it with the argument `max(z)` to create a
#'        a vector of colors and then use the z values as indices.
#'        If `col` is not NULL then a `col` column will be included in the final returned coordinates.
#' @param scale scale factor for values in matrix. Default = 1
#' @param solid Should the heightmap be made 'solid' i.e. without holes?
#'        This can be an expensive operation in terms of
#'        both memory and CPU, but should be OK for simple examples.
#'        Set to FALSE if things take too long or you will be rendering cuboids.
#'        This operation works by extruding cubes down from the top of the height map to the floor to
#'        ensure gaps do not appear when the slope is too great.
#' @param verbose Be verbose? default: FALSE
#' @param flipx,flipy Should the matrix be flipped in the horizontal/vertical directions (respectively)?
#'        Note: `flipy` defaults to `TRUE` as matrices are indexed
#'        from the top-down, but the coordinate space is increasing
#'        from the bottom up.   Flipping the matrix vertically is usually
#'        what you want.
#' @param ground Orientation of the ground plane. Default: "xy".  Possible
#'        values "xy", "xz", "zy"
#' @examples
#' if (require("grid")) {
#'   mat <- datasets::volcano
#'
#'   # Top view
#'   grid.newpage()
#'   grid.rect(gp=gpar(col=NA, fill="grey5"))
#'   width <- convertWidth(unit(0.007, "snpc"), "cm")
#'   pushViewport(viewport(width = 0.7, height = 0.7, x = 0.65, y = 0.65))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = grDevices::terrain.colors,
#'                           scale = 0.3, ground = "xy")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # South view
#'   pushViewport(viewport(width = 0.7, height = 0.3, x = 0.65, y = 0.15))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = grDevices::terrain.colors,
#'                           scale = 0.3, ground = "xz")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # West view
#'   pushViewport(viewport(width = 0.3, height = 0.7, x = 0.15, y = 0.65))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = grDevices::terrain.colors,
#'                           scale = 0.3, ground = "zy")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#' }
#' @export
xyz_heightmap <- function(mat, col = NULL, scale = 1, flipx = FALSE, flipy = TRUE,
                             ground = "xy", solid = TRUE, verbose = FALSE) {

  verbose <- isTRUE(verbose)
  solid   <- isTRUE(solid)

  ground <- match.arg(ground, c("xy", "xz", "zy"))

  # Sanity check matrix sizes
  if (!is.null(col) && is.matrix(col)) {
    stopifnot(identical(dim(col), dim(mat)))
  }

  # Flip matrix horizontally
  if (isTRUE(flipx)) {
    mat <- mat[,rev(seq_len(ncol(mat)))]
    if (!is.null(col) && is.matrix(col)) {
      col <- col[,rev(seq_len(ncol(col)))]
    }
  }

  # Flip matrix vertically
  if (isTRUE(flipy)) {
    mat <- mat[rev(seq_len(nrow(mat))), ]
    if (!is.null(col) && is.matrix(col)) {
      col <- col[rev(seq_len(nrow(col))), ]
    }
  }

  # Create coordinates
  coords <- data.frame(
    x = rep(seq_len(ncol(mat)), each = nrow(mat)),
    y = rep(seq_len(nrow(mat)), times = ncol(mat)),
    z = round(as.vector(mat) * scale)
  )
  # Add color if a matrix
  if (!is.null(col) && is.matrix(col))
      coords$col <- as.vector(col)

  # Extrude the cubes down to the ground
  if (solid) {
    start <- nrow(coords)
    coords_list <- lapply(seq(max(coords$z)), function(zfloor) {
      df <- subset(coords, z >= zfloor)
      df$z <- zfloor
      df
    })

    coords <- do.call(rbind, coords_list)
    if (verbose) message("Making solid expands cube count from ", start, " to ", nrow(coords))
  }

  # Add color if a vector or function
  if (!is.null(col) && !is.matrix(col)) {
      if (is.function(col))
          col <- col(max(coords$z))
      if (max(coords$z) <= length(col))
          coords$col <- col[coords$z]
      else
          coords$col <- col[cut(coords$z, length(col), labels = FALSE)]
  }

  if (ground == 'xz') {
    # Swap y/z coordinates
    tmp      <- coords$z
    coords$z <- coords$y
    coords$y <- tmp
  } else if (ground == 'zy') {
    # Swap x/z coordinates
    tmp      <- coords$z
    coords$z <- coords$x
    coords$x <- tmp
  } else if (ground == 'xy') {
    # Do nothing
  }

  coords
}
