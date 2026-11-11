#' Default Value Coalescing Operator
#'
#' @param x Preferred value.
#' @param y Fallback value if \code{x} is NULL or length 0.
#' @return \code{x} or \code{y}.
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Declare symbols used in non-standard evaluation for data.table to prevent R CMD check NOTEs
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "use",
    "_step_id",
    "_group_id",
    "_repeats",
    "execution_step",
    ".N",
    ":="
  ))
}
