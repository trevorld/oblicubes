get_fill <- function(fill, x, gp) {
    if (!is.null(fill)) {
        fill
    } else if (hasName(gp, "fill")) {
        gp$fill
    } else if (hasName(x, "fill")) {
        x$fill
    } else if (hasName(x, "col")) { # isocubes::coords_heightmap()
        x$col
    } else {
        "grey90"
    }
}

merge_gpar <- function(gp1, gp2) {
    for (name in names(gp2))
        gp1[[name]] <- gp2[[name]]
    gp1
}

splice1 <- function(x, y = NULL, z = NULL) {
    if (is.null(y)) {
        x
    } else if (is.null(z)) {
        vec <- vector("numeric", length(x) + length(y))
        vec[rep(c(TRUE, FALSE), length.out = length(vec))] <- x
        vec[rep(c(FALSE, TRUE), length.out = length(vec))] <- y
        vec
    } else {
        vec <- vector("numeric", length(x) + length(y) + length(z))
        vec[rep(c(TRUE, FALSE, FALSE), length.out = length(vec))] <- x
        vec[rep(c(FALSE, TRUE, FALSE), length.out = length(vec))] <- y
        vec[rep(c(FALSE, FALSE, TRUE), length.out = length(vec))] <- z
        vec
    }
}

splice4 <- function(x, y = NULL, z = NULL) {
    if (is.null(y)) {
        x
    } else if (is.null(z)) {
        vec <- vector("numeric", length(x) + length(y))
        vec[rep(rep(c(TRUE, FALSE), each = 4L), length.out = length(vec))] <- x
        vec[rep(rep(c(FALSE, TRUE), each = 4L), length.out = length(vec))] <- y
        vec
    } else {
        vec <- vector("numeric", length(x) + length(y) + length(z))
        vec[rep(rep(c(TRUE, FALSE, FALSE), each = 4L), length.out = length(vec))] <- x
        vec[rep(rep(c(FALSE, TRUE, FALSE), each = 4L), length.out = length(vec))] <- y
        vec[rep(rep(c(FALSE, FALSE, TRUE), each = 4L), length.out = length(vec))] <- z
        vec
    }
}
