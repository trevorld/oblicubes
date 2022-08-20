# Derivatives of function originally from {isocubes}
# Originally MIT License: (c) 2022 mikefc@coolbutuseless.com
# Compared to `isocubes::coords_heightmap()` we don't try to remove hidden cubes
# Allow `col` to a be a vector or function.
# By default "up" is 'z' coordinates (which are always positive) and we support more "ground" values

if (getRversion() >= "2.15.1")  utils::globalVariables("z")

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
#'        If `col` is not NULL then a `fill` column will be included in the final returned coordinates.
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
#' @param min Minimum target `z` value.  If `NULL` ignore else we "translate"
#'            the z-values so the minimum z-value is equal to this value.
#' @return A data frame of `x`, `y`, `z`, `raw`, and possibly `fill` columns.
#'         The "raw" column is the (original) "z" column before any `scale`, `min`, and `ground`
#'         transformations have been performed (it may be repeated "down" if `solid = TRUE`).
#'         The "raw" column can be useful as the `fill` value in `ggplot2` plots especially
#'         when adding a legend.
#' @examples
#' if (require("grDevices") && require("grid")) {
#'   mat <- datasets::volcano
#'   mat <- 0.3 * (mat - min(mat)) + 1.0
#'
#'   grid.newpage()
#'   grid.rect(gp=gpar(col=NA, fill="grey5"))
#'   width <- convertWidth(unit(0.007, "snpc"), "cm")
#'
#'   # Top view
#'   pushViewport(viewport(width = 0.7, height = 0.7, x = 0.65, y = 0.65))
#'   coords <- xyz_heightmap(mat, col = terrain.colors, solid = FALSE)
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # South view
#'   pushViewport(viewport(width = 0.7, height = 0.3, x = 0.65, y = 0.15))
#'   coords <- xyz_heightmap(mat, col = terrain.colors, ground = "xz")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#'
#'   # West view
#'   pushViewport(viewport(width = 0.3, height = 0.7, x = 0.15, y = 0.65))
#'   coords <- xyz_heightmap(mat, col = terrain.colors, ground = "zy")
#'   grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
#'   popViewport()
#' }
#' if (require("grDevices") && require("ggplot2")) {
#'   data("volcano", package = "datasets")
#'   df <- xyz_heightmap(volcano, scale = 0.3, min = 1, solid = FALSE)
#'   g <- ggplot(df, aes(x, y, z = z, fill = raw)) +
#'          geom_oblicuboids(light = FALSE) +
#'          coord_fixed() +
#'          scale_fill_gradientn(name = "Height (m)", colours=terrain.colors(256)) +
#'          labs(x = "East (10m)", y = "North (10m)", title = "Maungawhau (`datasets::volcano`)")
#'   plot(g)
#' }
#' @export
xyz_heightmap <- function(mat, col = NULL,
                          scale = 1, min = NULL,
                          flipx = FALSE, flipy = TRUE, ground = "xy",
                          solid = TRUE, verbose = FALSE) {

  verbose <- isTRUE(verbose)
  solid   <- isTRUE(solid)

  ground <- match.arg(ground, c("xy", "xz", "zy"))

  # Sanity check matrix sizes
  if (!is.null(col) && is.matrix(col)) {
    stopifnot(identical(dim(col), dim(mat)))
  }
  if (!is.null(raw) && is.matrix(raw)) {
    stopifnot(identical(dim(raw), dim(mat)))
  }

  # Flip matrix horizontally
  if (isTRUE(flipx)) {
    mat <- mat[,rev(seq_len(ncol(mat)))]
    if (!is.null(col) && is.matrix(col)) {
      col <- col[,rev(seq_len(ncol(col)))]
    }
    if (!is.null(raw) && is.matrix(raw)) {
      raw <- raw[,rev(seq_len(ncol(raw)))]
    }
  }

  # Flip matrix vertically
  if (isTRUE(flipy)) {
    mat <- mat[rev(seq_len(nrow(mat))), ]
    if (!is.null(col) && is.matrix(col)) {
      col <- col[rev(seq_len(nrow(col))), ]
    }
    if (!is.null(raw) && is.matrix(raw)) {
      raw <- raw[rev(seq_len(nrow(raw))), ]
    }
  }

  # Create coordinates
  coords <- data.frame(
    x = rep(seq_len(ncol(mat)), each = nrow(mat)),
    y = rep(seq_len(nrow(mat)), times = ncol(mat)),
    z = round(as.vector(mat) * scale),
    raw = as.vector(mat)
  )
  if (!is.null(min))
      coords$z <- coords$z + (round(min, 0) - base::min(coords$z))

  # Add color if a matrix
  if (!is.null(col) && is.matrix(col))
      coords$fill <- as.vector(col)

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
          coords$fill <- col[coords$z]
      else
          coords$fill <- col[cut(coords$z, length(col), labels = FALSE)]
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
