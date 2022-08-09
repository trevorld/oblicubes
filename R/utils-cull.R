cull_top <- function(df) {
    df <- df[order(df$z, df$x, df$y), ]
    l_by <- by(df,
               list(df$x, df$y),
               function(d) {
                   d$keep <- d$keep | op_diff(d$z) != 1
                   d
               })
    do.call(rbind, l_by)
}
cull_north <- function(df) {
    df <- df[order(df$y, df$x, df$z), ]
    l_by <- by(df,
               list(df$x, df$z),
               function(d) {
                   d$keep <- d$keep | op_diff(d$y) != 1
                   d
               })
    do.call(rbind, l_by)
}
cull_south <- function(df) {
    df <- df[order(-df$y, df$x, df$z), ]
    l_by <- by(df,
               list(df$x, df$z),
               function(d) {
                   d$keep <- d$keep | op_diff(d$y) != -1
                   d
               })
    do.call(rbind, l_by)
}
cull_west <- function(df) {
    df <- df[order(-df$x, df$y, df$z), ]
    l_by <- by(df,
               list(df$y, df$z),
               function(d) {
                   d$keep <- d$keep | op_diff(d$x) != -1
                   d
               })
    do.call(rbind, l_by)
}
cull_east <- function(df) {
    df <- df[order(df$x, df$y, df$z), ]
    l_by <- by(df,
               list(df$y, df$z),
               function(d) {
                   d$keep <- d$keep | op_diff(d$x) != 1
                   d
               })
    do.call(rbind, l_by)
}
cull_face <- function(df, face) {
    switch(face,
           top = cull_top(df),
           north = cull_north(df),
           east = cull_east(df),
           south = cull_south(df),
           west = cull_west(df))
}

op_diff <- function(x) c(diff(x), 0)

# Cull cubes and sort according to oblique projection angle and scale
# If `scale > 0` we may only partially cull hidden cubes
visible_cubes <- function(df, angle, scale) {
    if (scale == 0) { # easy to cull hidden cubes if `scale == 0`
        df <- df[order(df$z, df$x, df$y), ]
        i_hidden <- rev(duplicated(rev(paste(df$x, df$y))))
        if (any(i_hidden))
            df <- df[-which(i_hidden), ]
    } else {
        df$keep <- FALSE
        for (face in get_faces(angle, scale))
            df <- cull_face(df, face)
        df <- op_sort(df[which(df$keep), ], angle)
    }
    df$keep <- NULL
    df
}
