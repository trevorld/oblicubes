to_radians <- function(t) pi * t / 180

z_factor_x <- function(scale, angle) scale * cos(to_radians(angle))
z_factor_y <- function(scale, angle) scale * sin(to_radians(angle))

north_x <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- -width * c(0.5, -0.5, -0.5, 0.5)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * (row[3] + z_vertices)
    }))
}
north_y <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + 0.5 * width + z_factor * (row[3] + z_vertices)
    }))
}
east_x <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + 0.5 * width + z_factor * (row[3] + z_vertices)
    }))
}
east_y <-  function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * (row[3] + z_vertices)
    }))
}
south_x <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- -width * c(0.5, -0.5, -0.5, 0.5)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * (row[3] + z_vertices)
    }))
}
south_y <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] - 0.5 * width + z_factor * (row[3] + z_vertices)
    }))
}
west_x <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] - 0.5 * width + z_factor * (row[3] + z_vertices)
    }))
}
west_y <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    z_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * (row[3] + z_vertices)
    }))
}
top_x <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * (row[3] + 0.5 * width)
    }))
}
top_y <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * (row[3] + 0.5 * width)
    }))
}
