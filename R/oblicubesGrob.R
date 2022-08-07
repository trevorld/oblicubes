#' 3D render cubes via an oblique projection
#'
#' `oblicubesGrob()` / `grid.oblicubes()` renders cubes using a 3D oblique projection.
#' `oblicubesGrob()` returns a grid grob object while
#' `grid.oblicubes()` also draws the grob to the graphic device.
#' As a special case may also render a 2D primary view orthographic projection.
#' @param x Integer vector of x coordinates (if necessary will be rounded to integers).
#'          May be a `data.frame` of x,y,z coordinates.
#' @param y Integer vector of y coordinates (if necessary will be rounded to integers).
#' @param z Integer vector of z coordinates (if necessary will be rounded to integers).
#' @param ... Passed to [grid::gpar()].  Will override any values set in `gp`.
#' @param scale Oblique projection foreshortening factor.
#'              0.5 corresponds to the \dQuote{cabinet projection}.
#'              1.0 corresponds to the \dQuote{cavalier projection}.
#'              0.0 corresponds to a \dQuote{primary view orthographic projection}.
#' @param angle Oblique projection angle.
#' @param fill Fill color(s) for the cubes.
#'             If `NULL` and `x` is a data frame with a `fill` or `col` column then we use that column;
#'             if no such column but `gp` has a `fill` value we use that;
#'             otherwise we fall back to "grey90".
#' @param xo,yo The origin of the oblique projection coordinate system in grid units.
#'               The default is to try to guess a \dQuote{good} value.
#' @param width Width of the cube's (non-foreshortened) sides.
#'              The default will be to try to guess a \dQuote{good} value.
#' @param default.units Default units for the `xo`, `yo`, and `width` arguments.
#' @param name A character identifier (for grid).
#' @param gp A \sQuote{grid} gpar object.  See [grid::gpar()].
#'           Will be merged with the values in `...` and the value of `fill`.
#' @param vp A \sQuote{grid} viewport object.  See [grid::viewport()].
#'
#' @examples
#' if (require("grid")) {
#'   # we support arbitrary oblique projection angles
#'   mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
#'   coords <- xyz_heightmap(mat)
#'   coords$fill <- c("red", "yellow", "green")[coords$z]
#'
#'   angles <- c(135, 90, 45, 180, 45, 0, -135, -90, -45)
#'   scales <- c(0.5, 0.5, 0.5, 0.5, 0.0, 0.5, 0.5, 0.5, 0.5)
#'   vp_x <- rep(1:3/3 - 1/6, 3)
#'   vp_y <- rep(3:1/3 - 1/6, each = 3)
#'   grid.newpage()
#'   for (i in 1:9) {
#'       pushViewport(viewport(x=vp_x[i], y=vp_y[i], width=1/3, height=1/3))
#'       grid.rect(gp = gpar(lty = "dashed"))
#'       grid.oblicubes(coords, width = 0.15, xo = 0.25, yo = 0.15,
#'                      angle = angles[i], scale = scales[i],
#'                      gp = gpar(lwd=4))
#'       if(i != 5)
#'           grid.text(paste("angle =", angles[i]), y=0.92, gp = gpar(cex = 1.2))
#'       else
#'           grid.text(paste("scale = 0"), y=0.92, gp = gpar(cex = 1.2))
#'       popViewport()
#'   }
#'
#'   # volcano example
#'   mat <- datasets::volcano
#'   val <- as.vector(mat)
#'   val <- round(255 * (val - min(val)) / diff(range(val)))
#'   col <- grDevices::terrain.colors(256)[val + 1L]
#'   dim(col) <- dim(mat)
#'   coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
#'                              scale = 0.3, ground = "xy")
#'   grid.oblicubes(coords)
#' }
#' @export
oblicubesGrob <- function(x, y = NULL, z = NULL,
                         ...,
                         fill = NULL,
                         scale = 0.5,
                         angle = 45,
                         xo = NULL,
                         yo = NULL,
                         width = NULL,
                         default.units = "snpc",
                         name = NULL,
                         gp = gpar(),
                         vp = NULL) {

    fill <- get_fill(fill, x, gp)
    if (is.data.frame(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
    }
    if (!is.integer(x))
        x <- round(x, 0)
    if (!is.integer(y))
        y <- round(y, 0)
    if (!is.integer(z))
        z <- round(z, 0)
    angle <- angle %% 360

    #### do more sophisticated defaults
    if (is.null(width)) {
        n <- max(x, y, z)
        if (n > 10)
            width <- 0.9 * unit(1 / n, "snpc")
        else
            width <- 0.6 * unit(1 / n, "snpc")
    }
    if (!is.null(width) && !inherits(width, "unit"))
        width <- unit(width, default.units)
    if (is.null(xo))
        xo <- -0.5 * width
    if (!inherits(xo, "unit"))
        xo <- unit(xo, default.units)
    if (is.null(yo))
        yo <- -0.5 * width
    if (!inherits(yo, "unit"))
        yo <- unit(yo, default.units)

    # convert to 'bigpts' for easier calculations
    xo <- convertX(xo, "bigpts", valueOnly = TRUE)
    yo <- convertY(yo, "bigpts", valueOnly = TRUE)
    width <- convertWidth(width, "bigpts", valueOnly = TRUE)

    gp <- merge_gpar(gp, gpar(...))

    df <- data.frame(x=x, y=y, z=z, fill=fill)
    df <- op_sort(df, angle)
    if (scale == 0) { # easy to cull hidden cubes if `scale == 0`
        i_hidden <- rev(duplicated(rev(paste(df$x, df$y))))
        if (any(i_hidden))
            df <- df[-which(i_hidden), ]
    }
    df <- op_transform(df, xo, yo, width) # rescale, translate

    if (scale == 0) {
        n_faces <- 1
    } else if (angle == 0 || angle == 90 || angle == 180 || angle == 270) {
        n_faces <- 2
    } else {
        n_faces <- 3
    }

    mat <- as.matrix(df[, c(1,2,3)])
    x_top <- top_x(mat, angle, scale, width)
    y_top <- top_y(mat, angle, scale, width)

    if (angle >= 45 && angle < 135) {
        x_two <- south_x(mat, angle, scale, width)
        y_two <- south_y(mat, angle, scale, width)
    } else if (angle >= 135 && angle < 225) {
        x_two <- east_x(mat, angle, scale, width)
        y_two <- east_y(mat, angle, scale, width)
    } else if (angle >= 225 && angle < 315) {
        x_two <- north_x(mat, angle, scale, width)
        y_two <- north_y(mat, angle, scale, width)
    } else if (angle >= 315 || angle < 45) {
        x_two <- west_x(mat, angle, scale, width)
        y_two <- west_y(mat, angle, scale, width)
    }

    if ((angle > 0 && angle < 45) || (angle >= 135 && angle < 180)) {
        x_three <- south_x(mat, angle, scale, width)
        y_three <- south_y(mat, angle, scale, width)
    } else if ((angle > 90 && angle < 135) || (angle >= 225 && angle < 270)) {
        x_three <- east_x(mat, angle, scale, width)
        y_three <- east_y(mat, angle, scale, width)
    } else if ((angle > 180 && angle < 225) || (angle >= 315 && angle < 360)) {
        x_three <- north_x(mat, angle, scale, width)
        y_three <- north_y(mat, angle, scale, width)
    } else if ((angle > 315 && angle < 360) || (angle >= 45 && angle < 90)) {
        x_three <- west_x(mat, angle, scale, width)
        y_three <- west_y(mat, angle, scale, width)
    }

    if (n_faces == 1) {
        x <- x_top
        y <- y_top
        gp$fill <- df$fill
    } else if (n_faces == 2) {
        x <- splice4(x_two, x_top)
        y <- splice4(y_two, y_top)
        gp$fill <- rep(df$fill, each = 2)
    } else {
        x <- splice4(x_three, x_two, x_top)
        y <- splice4(y_three, y_two, y_top)
        gp$fill <- rep(df$fill, each = 3)
    }
    polygonGrob(x=x, y=y,
                id.lengths = rep(4, length(gp$fill)),
                default.units = "bigpts",
                gp = gp,
                name = "oblicubes")
}

#' @rdname oblicubesGrob
#' @export
grid.oblicubes <- function(x, y = NULL, z = NULL,
                          ...,
                          fill = NULL,
                          scale = 0.5,
                          angle = 45,
                          xo = NULL, yo = NULL, width = NULL,
                          default.units = "snpc",
                          name = NULL,
                          gp = gpar(),
                          vp = NULL) {

    grob <- oblicubesGrob(x, y, z,
                          ...,
                          scale = scale, angle = angle,
                          fill = fill,
                          xo = xo, yo = yo, width = width,
                          default.units = default.units,
                          name = name, gp = gp, vp = vp)
    grid.draw(grob)
    invisible(grob)
}