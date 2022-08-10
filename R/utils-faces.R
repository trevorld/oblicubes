to_radians <- function(t) pi * t / 180
z_factor_x <- function(scale, angle) scale * cos(to_radians(angle))
z_factor_y <- function(scale, angle) scale * sin(to_radians(angle))

get_faces <- function(angle = 45, scale = 0.5) {
    if (scale == 0)
        "top"
    else if (angle == 0)
        c("top", "west")
    else if (angle < 90)
        c("top", "west", "south")
    else if (angle == 90)
        c("top", "south")
    else if (angle < 180)
        c("top", "south", "east")
    else if (angle == 180)
        c("top", "east")
    else if (angle < 270)
        c("top", "east", "north")
    else if (angle == 270)
        c("top", "north")
    else
        c("top", "north", "west")
}

face_x <- function(face, mat, angle, scale, width) {
    switch(face,
           top = top_x(mat, angle, scale, width),
           east = east_x(mat, angle, scale, width),
           west = west_x(mat, angle, scale, width),
           north = north_x(mat, angle, scale, width),
           south = south_x(mat, angle, scale, width)
    )
}
face_y <- function(face, mat, angle, scale, width) {
    switch(face,
           top = top_y(mat, angle, scale, width),
           east = east_y(mat, angle, scale, width),
           west = west_y(mat, angle, scale, width),
           north = north_y(mat, angle, scale, width),
           south = south_y(mat, angle, scale, width)
    )
}

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

face_x_oid <- function(face, mat, angle, scale, width) {
    switch(face,
           top = top_x_oid(mat, angle, scale, width),
           east = east_x_oid(mat, angle, scale, width),
           west = west_x_oid(mat, angle, scale, width),
           north = north_x_oid(mat, angle, scale, width),
           south = south_x_oid(mat, angle, scale, width)
    )
}
face_y_oid <- function(face, mat, angle, scale, width) {
    switch(face,
           top = top_y_oid(mat, angle, scale, width),
           east = east_y_oid(mat, angle, scale, width),
           west = west_y_oid(mat, angle, scale, width),
           north = north_y_oid(mat, angle, scale, width),
           south = south_y_oid(mat, angle, scale, width)
    )
}

north_x_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- -width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * c(0, 0, row[3], row[3])
    }))
}
north_y_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + 0.5 * width + z_factor * c(0, 0, row[3], row[3])
    }))
}
east_x_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + 0.5 * width + z_factor * c(0, 0, row[3], row[3])
    }))
}
east_y_oid <-  function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * c(0, 0, row[3], row[3])
    }))
}
south_x_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- -width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * c(0, 0, row[3], row[3])
    }))
}
south_y_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    as.numeric(apply(mat, 1, function(row) {
        row[2] - 0.5 * width + z_factor * c(0, 0, row[3], row[3])
    }))
}
west_x_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    as.numeric(apply(mat, 1, function(row) {
        row[1] - 0.5 * width + z_factor * c(0, 0, row[3], row[3])
    }))
}
west_y_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * c(0, 0, row[3], row[3])
    }))
}
top_x_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_x(scale, angle)
    x_vertices <- width * c(-0.5, -0.5, 0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[1] + x_vertices + z_factor * (row[3])
    }))
}
top_y_oid <- function(mat, angle, scale, width) {
    z_factor <- z_factor_y(scale, angle)
    y_vertices <- width * c(0.5, -0.5, -0.5, 0.5)
    as.numeric(apply(mat, 1, function(row) {
        row[2] + y_vertices + z_factor * (row[3])
    }))
}
