test_that("`oblicuboidsGrob()`", {
    skip_if_not_installed("grid")
    skip_if_not_installed("vdiffr")
    library("grid")
    library("vdiffr")

    expect_doppelganger("basic_op_angles", function() {
      angles <- c(135, 90, 45, 180, 45, 0, -135, -90, -45)
      scales <- c(0.5, 0.5, 0.5, 0.5, 0.0, 0.5, 0.5, 0.5, 0.5)
      mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
      coords <- xyz_heightmap(mat, col = c("red", "yellow", "green"))
      vp_x <- rep(1:3/3 - 1/6, 3)
      vp_y <- rep(3:1/3 - 1/6, each = 3)

      for (i in 1:9) {
          pushViewport(viewport(x=vp_x[i], y=vp_y[i], width=1/3, height=1/3))
          grid.rect(gp = gpar(lty = "dashed"))
          grid.oblicuboids(coords, width = 0.15, xo = 0.25, yo = 0.15,
                         angle = angles[i], scale = scales[i],
                         gp = gpar(lwd=4))
          if (i != 5)
              grid.text(paste("angle =", angles[i]), y=0.92, gp = gpar(cex = 1.2))
          else
              grid.text(paste("scale = 0"), y=0.92, gp = gpar(cex = 1.2))
          popViewport()
      }
    })
    expect_doppelganger("null", function() {
        grid.oblicuboids(data.frame(x = integer(0), y = integer(0), z = integer(0)))
    })
    expect_doppelganger("null2", function() {
        grid.oblicuboids(x = integer(0), y = integer(0), z = integer(0))
    })

    expect_doppelganger("test_default", function() {
        mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
        df <- xyz_heightmap(mat)
        grid.oblicuboids(df)
    })
})
