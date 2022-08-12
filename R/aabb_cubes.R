#' Calculate axis-aligned bounding box for cubes
#'
#' Calculate axis-aligned bounding box (AABB) for cubes
#' with and without an \dQuote{oblique projection}.
#'
#' The \dQuote{oblique projection} of a set of \eqn{(x,y,z)} points onto the xy-plane
#' is \eqn{(x + \lambda * z * cos(\alpha), y + \lambda * z * sin(\alpha))}
#' where \eqn{\lambda} is the scale factor and \eqn{\alpha} is the angle.
#'
#' @param df A data frame of cube information with (at least) the
#'        named columns \dQuote{x}, \dQuote{y}, and \dQuote{z}.
#' @inheritParams grid.oblicubes
#' @param ... Ignored
#' @return A named list of ranges with five named elements `x`, `y`, and `z` for
#'         the axis-aligned bounding cube
#'         in xyz-space plus `x_op` and `y_op` for the axis-aligned bounding box
#'         of the \dQuote{oblique projection} onto the xy plane.
#' @examples
#'  mat <- datasets::volcano
#'  mat <- 0.3 * (mat - min(mat)) + 1.0
#'  df <- xyz_heightmap(mat, col = grDevices::terrain.colors)
#'
#'  aabb_cubes(df, scale = 0)
#'  aabb_cubes(df, scale = 0.5, angle = 45)
#'  aabb_cubes(df, scale = 1, angle = -90)
#' @noRd
aabb_cubes <- function(df,
                       scale = 0.5,
                       angle = 45,
                       ...) {
    if (nrow(df) == 0) {
        return(list(x = c(NA_real_, NA_real_),
                    y = c(NA_real_, NA_real_),
                    z = c(NA_real_, NA_real_),
                    x_op = c(NA_real_, NA_real_),
                    y_op = c(NA_real_, NA_real_)))
    }

    x <- range(df$x) + c(-0.5, 0.5)
    y <- range(df$y) + c(-0.5, 0.5)
    z <- range(df$z) + c(-0.5, 0.5)
    xs <- c( 0.5, -0.5, -0.5, 0.5)
    ys <- c( 0.5, -0.5, -0.5, 0.5)
    zs <- c(-0.5, -0.5,  0.5, 0.5)
    x_op_offset <- range(op_project_x(xs, zs, scale, angle))
    y_op_offset <- range(op_project_y(ys, zs, scale, angle))
    x_op <- range(op_project_x(df$x, df$z, scale, angle)) + x_op_offset
    y_op <- range(op_project_y(df$y, df$z, scale, angle)) + y_op_offset

    list(x = x, y = y, z = z,
         x_op = x_op, y_op = y_op)
}

# z is top of cuboid (instead of center of cube)
aabb_cuboids <- function(df,
                       scale = 0.5,
                       angle = 45,
                       ...) {
    if (nrow(df) == 0) {
        return(list(x = c(NA_real_, NA_real_),
                    y = c(NA_real_, NA_real_),
                    z = c(NA_real_, NA_real_),
                    x_op = c(NA_real_, NA_real_),
                    y_op = c(NA_real_, NA_real_)))
    }
    x <- range(df$x) + c(-0.5, 0.5)
    y <- range(df$y) + c(-0.5, 0.5)
    z <- c(0, max(df$z))

    df <- rbind(df[, c("x", "y", "z")], # top cuboid
                data.frame(x = df$x, y = df$y, z = 0)) # bottom cuboid
    x_op <- range(op_project_x(df$x, df$z, scale, angle)) + c(-0.5, 0.5)
    y_op <- range(op_project_y(df$y, df$z, scale, angle)) + c(-0.5, 0.5)

    list(x = x, y = y, z = z,
         x_op = x_op, y_op = y_op)
}
