# Modified example from https://github.com/coolbutuseless/isocubes
test_that("`oblicubesGrob()`", {
    skip_if_not_installed("grid")
    skip_if_not_installed("vdiffr")
    library("grid")
    library("vdiffr")

    expect_doppelganger("basic_op_angles", function() {
      angles <- c(135, 90, 45, 180, 45, 0, -135, -90, -45)
      scales <- c(0.5, 0.5, 0.5, 0.5, 0.0, 0.5, 0.5, 0.5, 0.5)
      mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
      coords <- xyz_heightmap(mat)
      coords$fill <- c("red", "yellow", "green")[coords$z]
      vp_x <- rep(1:3/3 - 1/6, 3)
      vp_y <- rep(3:1/3 - 1/6, each = 3)

      for (i in 1:9) {
          pushViewport(viewport(x=vp_x[i], y=vp_y[i], width=1/3, height=1/3))
          grid.rect(gp = gpar(lty = "dashed"))
          grid.oblicubes(coords, width = 0.15, xo = 0.25, yo = 0.15,
                         angle = angles[i], scale = scales[i],
                         gp = gpar(lwd=4))
          if (i != 5)
              grid.text(paste("angle =", angles[i]), y=0.92, gp = gpar(cex = 1.2))
          else
              grid.text(paste("scale = 0"), y=0.92, gp = gpar(cex = 1.2))
          popViewport()
      }
    })

    skip_if_not_installed("datasets")
    expect_doppelganger("volcano_orthographic", function() {
        mat <- datasets::volcano
        val <- as.vector(mat)
        val <- round(255 * (val - min(val)) / diff(range(val)))
        col <- grDevices::terrain.colors(256)[val + 1L]
        dim(col) <- dim(mat)

        # Top view
        grid.rect(gp=gpar(col=NA, fill="grey5"))
        width <- convertWidth(unit(0.007, "snpc"), "cm")
        pushViewport(viewport(width = 0.7, height = 0.7, x = 0.65, y = 0.65))
        coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
                                   scale = 0.3, ground = "xy", solid=FALSE)
        grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
        popViewport()

        # South view
        pushViewport(viewport(width = 0.7, height = 0.3, x = 0.65, y = 0.15))
        coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
                                   scale = 0.3, ground = "xz")
        grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
        popViewport()

        # West view
        pushViewport(viewport(width = 0.3, height = 0.7, x = 0.15, y = 0.65))
        coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
                                   scale = 0.3, ground = "zy")
        grid.oblicubes(coords, scale = 0, width = width, gp = gpar(col=NA))
        popViewport()
    })
})
