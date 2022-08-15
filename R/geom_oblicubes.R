#' Draw 2D/3D cubes with ggplot2
#'
#' `geom_oblicubes()` creates a `ggplot2` geom that draws cubes.
#'
#' `geom_oblicubes()` requires a fixed scale coordinate system with an aspect
#' ratio of 1 as provided by `ggplot2::coord_fixed()`.
#'
#' @section Aesthetics:
#' `geom_oblicubes()` understands the following aesthetics (required aesthetics are in bold).
#' See [oblicubesGrob()] for more details.
#'
#' * **`x`**
#' * **`y`**
#' * `z`
#' * `fill`
#' * `colour`
#' * `linetype`
#' * `linewidth`
#'
#' @inheritParams ggplot2::geom_rect
#' @inheritParams oblicubesGrob
#' @param  ... Aesthetics, used to set an aesthetic to a fixed value.
#' @seealso `geom_oblicubes()` is a wrapper around [oblicubesGrob()].
#' @examples
#' if (require("ggplot2")) {
#'   mat <- datasets::volcano
#'   mat <- 0.3 * (mat - min(mat)) + 1.0
#'
#'   # We require an equal aspect ratio
#'   df <- xyz_heightmap(mat)
#'   g <- ggplot(df, aes(x, y, z = z, fill = z)) +
#'          geom_oblicubes(light = FALSE) +
#'          coord_fixed() +
#'          scale_fill_viridis_c()
#'   plot(g)
#'
#'   df <- xyz_heightmap(mat, col = grDevices::terrain.colors)
#'   g <- ggplot(df, aes(x, y, z = z, fill = fill)) +
#'          geom_oblicubes() +
#'          coord_fixed() +
#'          scale_fill_identity()
#'   plot(g)
#' }
#' @export
geom_oblicubes <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       angle = 45,
                       scale = 0.5,
                       light = darken_face,
                       show.legend = NA,
                       inherit.aes = TRUE) {

    if (!requireNamespace("ggplot2"))
        stop("`geom_oblicubes()` requires the suggested package `{ggplot2}`")

    # assert cfg is character
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomOblicubes,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            angle = angle,
            scale = scale,
            light = light,
            ...
        )
    )
}

# GeomOblicubes is defined in `.onLoad()` in `hooks.R` so {ggplot2} can be Suggests instead of Imports
# Because it is defined in `.onLoad()` {covr} can't see it even though it is implicitly tested
# in `geom_oblicubes()` tests
create_GeomOblicubes <- function() { # nocov start
    ggplot2::ggproto(
    "GeomOblicubes",
    ggplot2::Geom,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
        z = 1,
        fill = "grey90",
        colour = "black",
        linewidth = 0.5,
        linetype = 1
    ),
    draw_key = function(data, params, size) {
        data$alpha <- NA
        ggplot2::draw_key_polygon(data, params, size)
    },
    draw_panel = function(self, data, panel_params, coord,
                          scale, angle, light) {
        if (coord$is_free()) {
            stop("'geom_oblicubes()' will not work correctly if not using a fixed scale.")
        }
        if (hasName(coord, "ratio") && coord$ratio != 1) {
            stop("'geom_oblicubes()' will not work correctly if not using an aspect ratio of 1.")
        }
        coord <- coord$transform(data, panel_params)
        grob_cubes(coord, panel_params, scale, angle, light)
    },
    setup_data = function(data, params) {
        l <- aabb_cubes(data,
                        scale = params$scale,
                        angle = params$angle)
        data$xmin <- l$x_op[1]
        data$xmax <- l$x_op[2]
        data$ymin <- l$y_op[1]
        data$ymax <- l$y_op[2]
        data
    }
  )
} # nocov end

grob_cubes <- function(coord, panel_params, scale, angle, light) {
    grid::gTree(coord = coord,
                panel_params = panel_params,
                scale = scale, angle = angle, light = light,
                cl = "oblicubes_cubes_geom")
}

#' @import grid
#' @export
makeContent.oblicubes_cubes_geom <- function(x) {
    coord <- x$coord
    panel_params <- x$panel_params

    x_width <- 1/diff(panel_params$x.range)
    y_width <- 1/diff(panel_params$y.range)
    width <- convertWidth(unit(x_width, "npc"), "bigpts")
    xs <- coord$x / x_width + panel_params$x.range[1]
    y <- coord$y / y_width + panel_params$y.range[1]
    z <- coord$z
    xo <- -panel_params$x.range[1] * x_width
    xo <- convertX(unit(xo, "npc"), "bigpts")
    yo <- -panel_params$y.range[1] * y_width
    yo <- convertY(unit(yo, "npc"), "bigpts")
    gp <- gpar(
          col  = coord$colour,
          lwd  = coord$linewidth * ggplot2::.pt,
          lty  = coord$linetype
    )
    grob <- oblicubesGrob(xs, y, z,
                  xo = xo, yo = yo, width = width,
                  default.units = "bigpts",
                  fill = coord$fill, light = x$light,
                  scale = x$scale, angle = x$angle,
                  gp = gp)
    setChildren(x, gList(grob))
}
