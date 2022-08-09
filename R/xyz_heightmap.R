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
#' @param mat integer matrix. The matrix will be interpreted as cubes flat on the
#'        page, with the value in the matrix interpreted as the height above the page.
#' @param col matrix of colours the same dimensions as the \code{mat} argument.
#'        Default: NULL.   If \code{col} is not NULL,
#'        then a \code{col} column will be included in the final returned coordinates.
#' @param scale scale factor for values in matrix. Default = 1
#' @param solid Should the heightmap be made 'solid' i.e. without holes?
#'        default: TRUE.  This can be an expensive operation in terms of
#'        both memory and CPU, but should be OK for simple examples.
#'        Set to FALSE if things take too long.  This operation works by
#'        extruding cubes down from the top of the height map to the floor to
#'        ensure gaps do not appear when the slope is too great.
#' @param verbose Be verbose? default: FALSE
#' @param flipx,flipy Should the matrix be flipped in the horizontal/vertical directions (respectively)?
#'        Default: \code{flipx = FALSE}, \code{flipy = TRUE}.
#'
#'        Note: \code{flipy} defaults to \code{TRUE} as matrices are indexed
#'        from the top-down, but the isometric coordinate space is increasing
#'        from the bottom up.   Flipping the matrix vertically is usually
#'        what you want.
#' @param ground Orientation of the ground plane. Default: 'xy'.  Possible
#'        values 'xz', 'xy', 'zy'
#' @examples
#' if (require("grid")) {
#'   mat <- datasets::volcano
#'   val <- as.vector(mat)
#'   val <- round(255 * (val - min(val)) / diff(range(val)))
#'   col <- grDevices::terrain.colors(256)[val + 1L]
#'   dim(col) <- dim(mat)
#'
#'   # Top view
#'   grid.newpage()
#'   grid.rect(gp=gpar(col=NA, fill="grey5"))
#'   width <- convertWidth(unit(0.007, "snpc"), "cm")
#'   pushViewport(viewport(width = 0.7, height = 0.7, x = 0.65, y = 0.65))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
#'                              scale = 0.3, ground = "xy")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # South view
#'   pushViewport(viewport(width = 0.7, height = 0.3, x = 0.65, y = 0.15))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
#'                              scale = 0.3, ground = "xz")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # West view
#'   pushViewport(viewport(width = 0.3, height = 0.7, x = 0.15, y = 0.65))
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
#'                              scale = 0.3, ground = "zy")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#' }
#' @export
xyz_heightmap <- function(mat, col = NULL, scale = 1, flipx = FALSE, flipy = TRUE,
                             ground = 'xy', solid = TRUE, verbose = FALSE) {

  verbose <- isTRUE(verbose)
  solid   <- isTRUE(solid)

  # Sanity check matrix sizes
  if (!is.null(col)) {
    stopifnot(identical(dim(col), dim(mat)))
  }

  # Flip matrix horizontally
  if (isTRUE(flipx)) {
    mat <- mat[,rev(seq_len(ncol(mat)))]
    if (!is.null(col)) {
      col <- col[,rev(seq_len(ncol(col)))]
    }
  }

  # Flip matrix vertically
  if (isTRUE(flipy)) {
    mat <- mat[rev(seq_len(nrow(mat))),]
    if (!is.null(col)) {
      col <- col[rev(seq_len(nrow(col))),]
    }
  }

  # Create coordinates
  coords <- data.frame(
    x = rep(seq_len(ncol(mat)), each = nrow(mat)),
    y = rep(seq_len(nrow(mat)), times = ncol(mat)),
    z = round(as.vector(mat) * scale)
  )

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
