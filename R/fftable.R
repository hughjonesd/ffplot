
# TODO: why does by use factor levels which aren't in data, in rhs?
fave <- function(formula, data = parent.frame(), subset = NULL) {
  vars <- get_all_vars(formula, data) # this is the data we need.
  subset. <-if (! is.null(subset)) eval(substitute(subset), data) else TRUE
  vars <- vars[subset.,]
  names_rhs <- attr(terms(formula[-2L], keep.order = TRUE), "term.labels")
  lhs <- attr(terms(formula[-3L], keep.order = TRUE), "term.labels")
  if (length(lhs) > 1) stop("Left hand side of formula cannot have more than 1 term")

  # TODO add example to README with functions on RHS
  rhs <- lapply(names_rhs, function (x) {
    x <- eval(parse(text = x), vars)
    if (is.factor(x)) x <- droplevels(x)
    x
  })
  if (! all(sapply(rhs, length) == length(rhs[[1]]))) stop("Right hand side variables did not all have same length")
  result <- aggregate(1:nrow(vars), rhs, FUN = function (x) eval(parse(text = lhs), vars[x,]))
  names(result) <- c(names_rhs, lhs)
  result
}

# TODO: fix fftable!
#' @rdname fftable
#' @export
fftable.default <- function(data, ...) fftable.formula(..., data = data)

#' Create a table from a formula
#'
#' \code{fftable} splits \code{data} by unique values of \code{formula}'s right hand side, and
#' evaluates the left hand side for each subset.
#'
#' The default method simply passes its arguments on to \code{fftable.formula}, taking the first argument as
#' \code{data}. This plays nicely with \code{\link[dplyr]{dplyr-package}}.
#'
#' @param formula a formula object with exactly one term on the LHS and RHS.
#' @param data a data frame (or list or environment) containing the variables referred to in \code{formula}.
#' @param subset an optional vector specifying a subset of \code{data}.
#' @name fftable
#' @return
#' A data frame like that from \code{\link{aggregate}}.
#' @export
#'
#' @examples
#' fftable(mean(mpg) ~ gear, mtcars)
#' fftable(range(mpg) ~ gear, mtcars)
#' fftable(mpg ~ gear, mtcars)
#' fftable(cbind(range(mpg), quantile(mpg, c(0.25, 0.75))) ~ gear, mtcars)
#' \dontrun{
#' library(dplyr)
#' mtcars %>% fftable(range(mpg) ~ gear)
#' }
fftable.formula <- function(formula, data = parent.frame(), subset = NULL) fave(formula, data, subset)

#' @export
fftable <- function(x, ...) UseMethod("fftable")
