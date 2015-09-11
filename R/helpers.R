

merge.list <- function (x, y, ...) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  i = match(names(y), names(x))
  i = is.na(i)
  if (any(i))
    x[names(y)[which(i)]] = y[which(i)]
  x
}


#' Return confidence intervals of a variable
#'
#' @param x A vector
#' @param level Confidence level
#' @param na.rm Logical. Should missing values be removed?
#' @param df Degrees of freedom. By default this is \code{Inf} which is equivalent to assuming a normal distribution.
#'
#' @return A length-two vector giving a confidence interval.
#' If \code{x} is numeric, the confidence interval around the mean is calculated using the t distribution.
#'
#' If \code{x} is logical, or a factor with two levels, the binomial confidence interval is calculated for the
#' proportion of \code{TRUE} cases, or cases on the first level, using \code{\link{prop.test}}.
#'
#' @export
#' @name ci
#' @examples
#' ci(rnorm(10), 0.95)
#' a <- rnorm(10)
#' ci(a, 0.99)
#' ci(a, 0.99, df = 10)
ci.numeric <- function(x, level = 0.95, na.rm = TRUE, df = Inf) {
  if (na.rm) x <- na.omit(x)
  lx <- length(x)
  se <- sd(x)/sqrt(lx)
  mean(x) + c(- se, + se) * qt((1 + level)/2, df)
}


#' @rdname ci
#' @export
ci.default <- function(x, level = 0.95, na.rm = TRUE) {
  size <- length(x)
  if (na.rm) x <- x[! is.na(x)]
  fx <- as.factor(x)
  if (nlevels(droplevels(fx)) > 2) stop("Called ci() on a non-numeric variable with more than two unique values")
  if (! is.logical(x)) x <- x == levels(x)[1]
  prop.test(sum(x), length(x), conf.level = level)$conf.int
}


#' @rdname ci
#' @export
ci <- function(x, ...) UseMethod("ci")

#' Calculates the proportion of \code{TRUE} cases or cases matching the first level
#'
#' Unlike \code{mean}, if used inside \code{\link{ffplot}} this will automatically use a barplot
#' @param x a logical vector or a factor with only 2 levels
#'
#' @return A numeric
#' @export
prop <- function(x) {
  if (! (is.logical(x) || (is.factor(x) && nlevels(x) <= 2))) stop("prop() only works with logicals or 2-level factors")
  if (! is.logical(x)) x <- x == levels(x)[1]
  mean(x)
}

