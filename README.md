<!-- # oblicubes <img src="man/figures/logo.png" align="right" width="200px" alt="oblicubes hex sticker"> -->
# oblicubes

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/oblicubes)](https://cran.r-project.org/package=oblicubes)
[![R-CMD-check](https://github.com/trevorld/oblicubes/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/oblicubes/actions)
[![Coverage Status](https://codecov.io/gh/trevorld/oblicubes/branch/main/graph/badge.svg?token=WV34OH5BI9)](https://codecov.io/gh/trevorld/oblicubes)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)

  + [Different oblique projections](#oblique)
  + [Volcano heightmap](#volcano)
  + [Bitmap font](#bitmap)

## <a name="overview">Overview</a>

`{oblicubes}` is an extension for [coolbutuseless's](https://github.com/coolbutuseless) [{isocubes}](https://github.com/coolbutuseless/isocubes) that supports 3D graphics in `{grid}` by rendering cubes with an [oblique projection](https://en.wikipedia.org/wiki/Oblique_projection) (instead of an [isometric projection](https://en.wikipedia.org/wiki/Isometric_projection)).  As a special case we also support "primary view orthographic projections" as well.  Like `{isocubes}` the `{oblicubes}` package only supports rendering non-rotated cubes placed at integer coordinates.  If you need to do more complex oblique projections you'll need to use a package like [{piecepackr}](https://github.com/piecepackr/piecepackr) which supports additional shapes, supports adding art/text to their faces, rotating shapes, placing shapes at non-integer coordinates, etc.

| `{oblicubes}` | `{isocubes}` |
|---|---|
| [oblique projection](https://en.wikipedia.org/wiki/Oblique_projection), "primary view orthographic projection" | [isometric projection](https://en.wikipedia.org/wiki/Isometric_projection) | 
| right-handed coordinate system with z vertical  | left-handed coordinate system with y vertical |
| Use `xyz_heightmap()` to create x,y,z coordinates | Use `coord_heightmap()` to create x,y,z coordinates |
| Use `oblicubesGrob()`, `grid.oblicubes()` to render image | Use `isocubesGrob()` to render image |
| Fast culling of non-visible cubes for "primary view orthographic projection".  Slower and less thorough culling of non-visible cubes for "oblique projection". | Fast culling of non-visible cubes. |

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/oblicubes")
```

## <a name="examples">Examples</a>



### <a name="oblique">Different oblique projections</a>

`{oblicubes}` supports different oblique projection angles:


```r
library("grid")
library("oblicubes")
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
```

![](man/figures/README-angles-1.png)

### <a name="volcano">Volcano heightmap</a>


```r
mat <- datasets::volcano
val <- as.vector(mat)
val <- round(255 * (val - min(val)) / diff(range(val)))
col <- grDevices::terrain.colors(256)[val + 1L]
dim(col) <- dim(mat)
```

By default we do an oblique projection with a `scale` of 0.5 and an `angle` of 45.  This is also known as a "cabinet projection":


```r
coords <- xyz_heightmap(mat - min(mat) + 3L, col = col,
                           scale = 0.3, ground = "xy")
grid.oblicubes(coords)
```

![](man/figures/README-volcano_oblique-1.png)

* By playing around with the `flipx`, `flipy`, `ground` arguments in `xyz_coords` it is also possible to generate views from different sides of the object.  
* A `scale` of 0 gives you a "primary view orthographic projection".


```r
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
```

![](man/figures/README-volcano_orthographic-1.png)

### <a name="bitmap">Bitmap fonts</a>


```r
library("bittermelon")
```

```
## 
## Attaching package: 'bittermelon'
```

```
## The following object is masked from 'package:base':
## 
##     which
```

```r
font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
bml <- as_bm_list("RSTATS", font = font)
# Add a shadow effect and border
bm <- (3 * bml) |>
    bm_pad(sides = 2L) |>
    bm_shadow(value = 2L) |>
    bm_call(cbind) |>
    bm_extend(sides = 1L, value = 1L)
col <- matrix("", nrow=nrow(bm), ncol=ncol(bm))
col[which(bm == 3)] <- "darkblue"
col[which(bm == 2)] <- "lightblue"
col[which(bm == 1)] <- "grey20"
col[which(bm == 0)] <- "white"

coords <- xyz_heightmap(bm, col, flipy=FALSE)
grid.oblicubes(coords, width=unit(2.2, "mm"))
```

![](man/figures/README-bitmap_font-1.png)
