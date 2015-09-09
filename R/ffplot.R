
#' @import ggplot2
#' @import reshape2


# TODO: why does by use factor levels which aren't in data, in rhs?
fave <- function(formula, data = parent.frame(), subset = NULL) {
  subset2 <-if (! is.null(subset)) eval(substitute(subset), data) else TRUE
  vars <- get_all_vars(formula, data) # this is the data we need.
  vars <- vars[subset2,]
  rhs <- attr(terms(formula[-2L], keep.order = TRUE), "term.labels")
  lhs <- attr(terms(formula[-3L], keep.order = TRUE), "term.labels")
  if (length(rhs) > 1) stop("Right hand side of formula has more than 1 term: ", paste(rhs, collapse = ", "))
  if (length(lhs) > 1) stop("Left hand side of formula has more than 1 term: ", paste(lhs, collapse = ", "))

  rhs <- eval(parse(text = rhs), vars)
  if (is.factor(rhs)) rhs <- droplevels(rhs)
  result <- by(vars, rhs, function (x) eval(parse(text = lhs), x), simplify = FALSE)
  structure(result, class = "list", colvalues = sort(unique(rhs)))
}

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
#' An object of class \code{\link{table}}, or \code{\link{ftable}} if the result has more than 2 dimensions.
#' @export
#'
#' @examples
#' fftable(mean(mpg) ~ gear, mtcars)
#' fftable(range(mpg) ~ gear, mtcars)
#' # returns a list:
#' fftable(mpg ~ gear, mtcars)
#' # returns a flat table:
#' fftable(cbind(range(mpg), quantile(mpg, c(0.25, 0.75))) ~ gear, mtcars)
#' \dontrun{
#' library(dplyr)
#' mtcars %>% fftable(range(mpg) ~ gear)
#' }
fftable.formula <- function(formula, data = parent.frame(), subset = NULL) {
  res <- fave(formula, data, subset)
  res <- simplify2array(res)
  if (! is.list(res) && length(dim(res[[1]])) > 1) {
    return (ftable(as.table(res)))
  } else {
    return(as.table(res))
  }
}

#' @export
fftable <- function(x, ...) UseMethod("fftable")

geom_map <- list(
  ci = "errorbar",
  range = "linerange"
)

#' Return confidence intervals of a variable
#'
#' @param x A numeric vector
#' @param level Confidence level
#' @param na.rm Logical. Should missing values be removed?
#' @param df Degrees of freedom. By default this is \code{Inf} which is equivalent to assuming a normal distribution.
#'
#' @return A length-two vector giving the confidence interval around the mean.
#' @export
#'
#' @examples
#' ci(rnorm(10), 0.95)
#' a <- rnorm(10)
#' ci(a, 0.99)
#' ci(a, 0.99, df = 10)
ci <- function(x, level = 0.95, na.rm = TRUE, df = Inf) {
  if (na.rm) x <- na.omit(x)
  lx <- length(x)
  se <- sd(x)/sqrt(lx)
  mean(x) + c(- se, + se) * qt((1 + level)/2, df)
}

#' Fast Friendly Plot
#'
#' Plots subsets of your data using \code{\link[ggplot2]{ggplot}}.
#'
#' The data is split into subsets for each unique value of the right hand side of \code{formula}.
#' Each term on the left hand side of \code{formula} is evaluated within that subset, and the results
#' are used to create a \code{\link[ggplot2]{layer}}.
#'
#' By default, \code{geom}s are chosen as follows: if the left hand side term evaluates to a character or factor,
#' a histogram of proportions is plotted using \code{\link[ggplot2]{geom_histogram}}. If the term is numeric and
#' always returns a single value (e.g. \code{mean(y)}) then a line is plotted; if it always returns two values,
#' vertical lines are used; otherwise points are plotted. Certain functions are automagically recognized, e.g.
#' \code{\link{ci}} creates a \code{\link[ggplot2]{geom_errorbar}}.
#'
#' @param formula a two-sided formula. The right hand side must contain only one term (which can be an interaction, e.g. \code{g1:g2}).
#' @param data a data frame
#' @param geom a vector of one or more names of geoms
#' @param ... other arguments passed into (all) geoms
#' @param subset an optional vector specifying a subset of the data
#' @param smooth logical (not yet implemented!)
#'
#' @return A ggplot object which can be printed or modified.
#' @export
#'
#' @examples
#' data(diamonds)
#' d30 <- diamonds[1:30,]
#' ffplot(price ~ carat, d30)
#' ffplot(price ~ color, d30)
#' # histogram:
#' ffplot(cut ~ color, d30)
#' ffplot(range(price) ~ color, d30)
#' ffplot(ci(price, 0.95) ~ color, d30)
#' ffplot(price + mean(price) ~ color, d30)
#' # customizing geoms:
#' ffplot(mean(price) + ci(price, .99) ~ color, d30, geom = c("point", "linerange"))
#' ffplot(price ~ color, diamonds, geom ="violin")
#' ffplot(mean(price) ~ color, diamonds, geom = "point", shape = 3)
#' # extra customization with ggplot:
#' ffplot(cut ~ color, diamonds) + scale_fill_grey()
ffplot <- function(formula, data = parent.frame(), geom = NULL,  ..., subset = NULL, smooth = NULL) {
  rhs <- attr(terms(formula[-2L], keep.order = TRUE), "term.labels")
  lhs_all <- attr(terms(formula[-3L], keep.order = TRUE), "term.labels")
  if (! missing(geom)) geom <- rep_len(geom, length(lhs_all))

  # TODO: facetting. This is complex because you need to (a) include facet info in (all) datasets
  # (b) interact the facets with the RHS when doing calculations.
  # (c) rewrite fave (or whatever) to return tables separately
  ggp <- ggplot()
  i <- 0
  for (lhs in lhs_all) {
    i <- i + 1
    fml <- formula(paste(lhs, "~", rhs))

    result <- fave(fml, data, subset)
    result_data <- unlist(result)
    lhsfun <- parse(text = lhs)[[1]]
    if (! is.name(lhsfun)) lhsfun <- lhsfun[[1]] # if it's a name it's probably just a variable
    lhsfun <- as.character(lhsfun)
    lenresult <- sapply(result, length)
    cv <- attr(result, "colvalues") # can be numeric, factor
    geom_name <- if (! is.null(geom)) geom[i] else
      if (! is.null(geom_map[[lhsfun]])) geom_map[[lhsfun]] else
      if (is.character(result_data) || is.factor(result_data)) ifelse(is.numeric(cv), "density", "histogram") else
      if (all(lenresult == 1)) "line" else
      if (all(lenresult == 2)) "linerange" else "point"

    # TODO: allow default attributes to be overridden by ..., e.g. position = "fill" below
    dfr <- data.frame(y = result_data, x = rep(cv, lenresult), count = unlist(lapply(lenresult, seq_len)))
    geom_names <- c("point", "line", "errorbar", "boxplot", "barplot", "linerange", "violin", "histogram", "density")
    geom_name <- match.arg(geom_name, geom_names)
    lyr <- switch(geom_name,
      "point"     = geom_point(aes(x = x, y = y), data = dfr, size = 3, ...),
      "line"      = geom_line(aes(x = x, y = y, group = 1), data = dfr, ...),
      "boxplot"   = geom_boxplot(aes(x = factor(x), y = y), data = dfr, ...),
      "barplot"   = geom_bar(aes(x = x, y = y), stat = "identity", data = dfr, fill = "navy", ...),
      "histogram" = geom_histogram(aes(x = x, group = y, fill = y), data = dfr, position = "fill"),
      "density"   = geom_density(aes(x = x, group = y, fill = y), data = dfr, position = "fill"),
      "violin"    = geom_violin(aes(x = factor(x), y = y), data = dfr, ...),
      "errorbar"  = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
                      geom = "errorbar", width = 0.25, data = dfr, ...),
      "linerange" = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
                      geom = "linerange", data = dfr, ...)
    )

    ggp <- ggp + lyr
    if (geom_name %in% c("histogram", "density")) ggp <- ggp + guides(fill = guide_legend(title = lhs))
  }
  ggp <- ggp + xlab(rhs) + ylab(Reduce(paste, deparse(formula[[2]])))
  return(ggp)
}
