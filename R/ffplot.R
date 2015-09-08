
#' @import ggplot2
#' @import reshape2

fave <- function(x, ...) UseMethod("fave")

# TODO: why does by use factor levels which aren't in data, in rhs?
fave.formula <- function(formula, data = parent.frame(), subset = NULL) {
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

fave.default <- function(data, ...) fave.formula(..., data = data)

# print.fave <- function(x) print(simplify2array(x))

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
  geom_names <- c("point", "line", "errorbar", "boxplot", "barplot", "linerange", "violin", "histogram")
  if (! missing(geom)) geom <- rep_len(geom, length(lhs_all))

  ggp <- ggplot(data = NULL)
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
    # don't use "line" if rhs is a factor (or fix ggplot complaint) TODO
    geom_name <- if (! is.null(geom)) geom[i] else
      if (! is.null(geom_map[[lhsfun]])) geom_map[[lhsfun]] else
      if (is.character(result_data) || is.factor(result_data)) "histogram" else
      if (all(lenresult == 1)) "line" else
      if (all(lenresult == 2)) "linerange" else "point"

    # TODO: allow default attributes to be overridden by ..., e.g. position = "fill" below
    cv <- attr(result, "colvalues") # can be numeric, factor
    dfr <- data.frame(y = result_data, x = rep(cv, lenresult), count = unlist(lapply(lenresult, seq_len)))
    geom_name <- match.arg(geom_name, geom_names)
    lyr <- switch(geom_name,
      "point"     = geom_point(aes(x = x, y = y), data = dfr, size = 3, ...),
      "line"      = geom_line(aes(x = x, y = y, group = 1), data = dfr, ...),
      "boxplot"   = geom_boxplot(aes(x = factor(x), y = y), data = dfr, ...),
      "barplot"   = geom_bar(aes(x = x, y = y), stat = "identity", data = dfr, fill = "navy", ...),
      "histogram" = geom_histogram(aes(x = x, group = y, fill = y), data = dfr, position = "fill"),
      "violin"    = geom_violin(aes(x = factor(x), y = y), data = dfr, ...),
      "errorbar"  = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
                      geom = "errorbar", width = 0.25, data = dfr, ...),
      "linerange" = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
                      geom = "linerange", data = dfr, ...)
    )

    ggp <- ggp + lyr
    if (geom_name == "histogram") ggp <- ggp + guides(fill = guide_legend(title = lhs))
  }
  ggp <- ggp + xlab(rhs) + ylab(Reduce(paste, deparse(formula[[2]])))
  return(ggp)
}
