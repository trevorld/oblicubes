test_that("`geom_oblicuboids()`", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("vdiffr")
    library("ggplot2")
    library("vdiffr")
    mat <- matrix(c(1, 2, 1, 2, 3, 2, 1, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
    df <- xyz_heightmap(mat)

    expect_doppelganger("geom_oblicuboids",  {
      ggplot(df, aes(x, y, z = z, fill = z)) +
          geom_oblicuboids(light = FALSE) +
          coord_fixed() +
          scale_fill_viridis_c()
    })
})
