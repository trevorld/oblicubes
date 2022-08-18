#' Render 2D/3D cuboids via an oblique projection
#'
#' `oblicuboidsGrob()` / `grid.oblicuboids()` renders cuboids using a 3D oblique projection.
#' `oblicuboidsGrob()` returns a grid grob object while
#' `grid.oblicuboids()` also draws the grob to the graphic device.
#' As a special case may also render a 2D primary view orthographic projection.
#' @inheritParams oblicubesGrob
#' @param x Integer vector of x coordinates (if necessary will be rounded to integers).
#'          May be a `data.frame` of x,y,z coordinates.
#'          This will be the x-value at the *center* of the cuboid.
#' @param y Integer vector of y coordinates (if necessary will be rounded to integers).
#'          This will be the x-value at the *center* of the cuboid.
#' @param z Integer vector of z coordinates (if necessary will be rounded to integers).
#'          This will be the z-value at the *top* of the cuboid.
#' @param fill Fill color(s) for the cuboids.
#'             If `NULL` and `x` is a data frame with a `fill` or `col` column then we use that column;
#'             if no such column but `gp` has a `fill` value we use that;
#'             otherwise we fall back to "grey90".
#' @param width Width of the cuboids's (non-foreshortened) side.
#'              The default will be to try to guess a \dQuote{good} value.
#'
#' @examples
#' if (require("grid")) {
#'   # we support arbitrary oblique projection angles
#'   mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
#'   coords <- xyz_heightmap(mat, col = c("red", "yellow", "green"),
#'                           solid = FALSE)
#'   angles <- c(135, 90, 45, 180, 45, 0, -135, -90, -45)
#'   scales <- c(0.5, 0.5, 0.5, 0.5, 0.0, 0.5, 0.5, 0.5, 0.5)
#'   vp_x <- rep(1:3/3 - 1/6, 3)
#'   vp_y <- rep(3:1/3 - 1/6, each = 3)
#'   grid.newpage()
#'   for (i in 1:9) {
#'       pushViewport(viewport(x=vp_x[i], y=vp_y[i], width=1/3, height=1/3))
#'       grid.rect(gp = gpar(lty = "dashed"))
#'       grid.oblicuboids(coords, width = 0.15, xo = 0.25, yo = 0.15,
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
#'   mat <- 0.3 * (mat - min(mat)) + 1.0
#'   coords <- xyz_heightmap(mat, col = grDevices::terrain.colors,
#'                           solid = FALSE)
#'   grid.newpage()
#'   grid.oblicuboids(coords)
#' }
#' @return A grid grob.  As a side effect `grid.oblicubes()` also draws to the active graphics device.
#' @export
oblicuboidsGrob <- function(x, y = NULL, z = NULL,
                         ...,
                         fill = NULL,
                         light = darken_face,
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
    if (!is.numeric(z))
        z <- as.numeric(z)
    angle <- angle %% 360

    if (is.null(width) || is.null(xo) || is.null(yo))
        l <- aabb_cuboids(data.frame(x=x, y=y, z=z), scale = scale, angle = angle)
    else
        l <- NULL

    gp <- merge_gpar(gp, gpar(...))

    df <- data.frame(x=x, y=y, z=z, fill=fill)
    df <- visible_cuboids(df, angle, scale) # cull, sort

    # Delay draw so xo, yo, width correctly converted in relevant viewport
    gTree(df=df,
          xo=xo, yo=yo, width=width, l=l,
          angle=angle, scale=scale, light=light,
          default.units = default.units,
          name = name, gp = gp, vp = vp,
          cl = "oblicubes_cuboids_grob")
}

#' @export
makeContent.oblicubes_cuboids_grob <- function(x) {
    angle <- x$angle
    scale <- x$scale
    xo <- x$xo
    yo <- x$yo
    width <- x$width
    l <- x$l

    if (is.null(width)) {
        x_diff <- diff(l$x_op)
        y_diff <- diff(l$y_op)
        width <- 0.95 * min(convertWidth(unit(1 / x_diff, "npc"), "bigpts"),
                            convertHeight(unit(1 / y_diff, "npc"), "bigpts"))
    }
    if (!inherits(width, "unit"))
        width <- unit(width, x$default.units)
    if (is.null(xo))
        xo <- (l$x[1]-l$x_op[1]-0.5) * width + unit(0.01, "snpc")
    if (!inherits(xo, "unit"))
        xo <- unit(xo, x$default.units)
    if (is.null(yo))
        yo <- -l$y_op[1] * width + unit(0.01, "snpc")
    if (!inherits(yo, "unit"))
        yo <- unit(yo, x$default.units)

    # convert to 'bigpts' for easier calculations
    xo <- convertX(xo, "bigpts", valueOnly = TRUE)
    yo <- convertY(yo, "bigpts", valueOnly = TRUE)
    width <- convertWidth(width, "bigpts", valueOnly = TRUE)

    df <- op_transform(x$df, xo, yo, width) # rescale, translate
    mat <- as.matrix(df[, c(1, 2, 3)])

    faces <- get_faces(angle, scale)
    xs <- lapply(faces, face_x_oid, mat, angle, scale, width)
    ys <- lapply(faces, face_y_oid, mat, angle, scale, width)

    xs <- do.call(splice4, xs)
    y <- do.call(splice4, ys)
    gp <- gpar()
    gp$fill <- compute_fill(df$fill, faces, x$light)

    grob <- polygonGrob(x=xs, y=y,
                        id.lengths = rep(4, length(gp$fill)),
                        default.units = "bigpts", gp = gp, name = "cuboids")
    setChildren(x, gList(grob))
}

#' @rdname oblicuboidsGrob
#' @export
grid.oblicuboids <- function(x, y = NULL, z = NULL,
                          ...,
                          fill = NULL,
                          scale = 0.5,
                          angle = 45,
                          xo = NULL, yo = NULL, width = NULL,
                          default.units = "snpc",
                          name = NULL,
                          gp = gpar(),
                          vp = NULL) {

    grob <- oblicuboidsGrob(x, y, z,
                          ...,
                          scale = scale, angle = angle,
                          fill = fill,
                          xo = xo, yo = yo, width = width,
                          default.units = default.units,
                          name = name, gp = gp, vp = vp)
    grid.draw(grob)
    invisible(grob)
}
