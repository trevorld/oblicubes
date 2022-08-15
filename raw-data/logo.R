library("bittermelon")
library("grid")
library("oblicubes")
library("piecepackr")

font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
font <- read_yaff(font_file)
obli <- as_bm_list("oblicubes", font = font) |> bm_call(cbind)
coords <- xyz_heightmap(obli, flipy=FALSE)

font <- hexfont::unifont()
emoji <- as_bm_list("🐭", font = font) |> bm_call(cbind)
coords_emoji <- xyz_heightmap(emoji, flipy=FALSE)

draw_logo <- function() {
    hex <- pp_shape("convex6")
    grid.newpage()
    grid.draw(hex$shape(gp = gpar(col = NA, fill = "white")))
    vp <- viewport(x=0.52, y=0.75, height=0.3, width=0.8)
    grid.oblicubes(coords, vp=vp, scale = 0.7)

    vp <- viewport(x=0.53, y=0.34, height=0.4, width=0.5)
    grid.oblicubes(coords_emoji, vp=vp, scale = 0.7)

    grid.draw(hex$mat(mat_width = 0.03, gp = gpar(col = NA, fill = "black")))
}

w <- 3.0
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in",
    res = 72, bg = "transparent")
draw_logo()
dev.off()
