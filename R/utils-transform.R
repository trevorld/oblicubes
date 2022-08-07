op_sort <- function(df, angle=45) {
    angle <- angle %% 360
    if ((0 <= angle) && (angle < 90)) {
        df <- df[order(df$z, -df$y, -df$x), ]
    } else if ((90 <= angle) && (angle < 180)) {
        df <- df[order(df$z, -df$y, df$x), ]
    } else if ((180 <= angle) && (angle < 270)) {
        df <- df[order(df$z, df$y, df$x), ]
    } else {
        df <- df[order(df$z, df$y, -df$x), ]
    }
    df
}

op_transform <- function(df, xo, yo, width) {
    df$x <- width * df$x
    df$y <- width * df$y
    df$z <- width * df$z
    df$x <- df$x + xo
    df$y <- df$y + yo
    df
}
