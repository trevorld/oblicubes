get_fill <- function(fill, x, gp) {
    if (!is.null(fill)) {
        fill
    } else if (hasName(x, "fill")) {
        x$fill
    } else if (hasName(x, "col")) { # isocubes::coords_heightmap
        x$col
    } else if (hasName(gp, "fill")) {
        gp$fill
    } else {
        "grey90"
    }
}

merge_gpar <- function(gp1, gp2) {
    for (name in names(gp2))
        gp1[[name]] <- gp2[[name]]
    gp1
}

splice4 <- function(x, y, z = NULL) {
    vec <- vector("numeric", length(x) + length(y) + length(z))
    if (is.null(z)) {
        vec[rep(rep(c(TRUE, FALSE), each = 4L), length.out = length(vec))] <- x
        vec[rep(rep(c(FALSE, TRUE), each = 4L), length.out = length(vec))] <- y
    } else {
        vec[rep(rep(c(TRUE, FALSE, FALSE), each = 4L), length.out = length(vec))] <- x
        vec[rep(rep(c(FALSE, TRUE, FALSE), each = 4L), length.out = length(vec))] <- y
        vec[rep(rep(c(FALSE, FALSE, TRUE), each = 4L), length.out = length(vec))] <- z
    }
    vec
}
