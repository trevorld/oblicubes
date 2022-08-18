#' @import grid
#' @importFrom utils hasName
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x
