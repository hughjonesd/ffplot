
#' @import ggplot2
#' @import reshape2
#' @import Formula


geom_map <- list(
  ci = "geom_errorbar",
  prop = "geom_bar"
)

geom_names <- c("point", "line", "errorbar", "boxplot", "bar", "linerange", "violin", "histogram", "density", "smooth",
  "freqpoly", "jitter", "quantile", "text")

extra_args <- list(
  geom_errorbar    = list(stat = "summary", fun.y = mean, fun.ymin = min, fun.ymax = max, width = 0.5),
  geom_linerange   = list(stat = "summary", fun.y = mean, fun.ymin = min, fun.ymax = max),
  geom_density     = list(position = "stack"),
  geom_bar         = list(stat = "identity"),
  geom_smooth      = list(se = FALSE)
)

extra_mapping <- list(
  geom_line      = list(group = "1"),
  geom_smooth    = list(group = "1"),
  geom_errorbar  = list(group = "x"),
  geom_linerange = list(group = "x"),
  geom_boxplot   = list(x = "factor(x)"),
  geom_violin    = list(x = "factor(x)"),
  geom_histogram = list(group = "y", fill = "y", y = NULL),
  geom_freqpoly  = list(group = "y", color = "y", y = NULL),
  geom_density   = list(group = "y", fill = "y", y = NULL, alpha = 0.3),
  geom_quantile  = list(colour = "factor(..quantile..)")
)

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
#' The default method simply calls \code{ffplot.formula}, passing its first argument as \code{data}. This plays nicely
#' with \code{\link[dplyr]{dplyr}}.
#'
#' @param formula a two-sided formula. The right hand side must contain only one term (which can be an interaction, e.g. \code{g1:g2}).
#' @param data a data frame
#' @param ... other arguments passed into (all) geoms
#' @param subset an optional vector specifying a subset of the data
#' @param smooth logical (not yet implemented!)
#'
#' @return A ggplot object which can be printed or modified.
#' @export
#'
#' @name ffplot
#' @examples
#' data(diamonds)
#' d30 <- diamonds[1:30,]
#'
#' # numeric y variables:
#' ffplot(price ~ carat, d30)
#' ffplot(price ~ color, d30)
#'
#' # non-numeric y:
#' ffplot(cut ~ color, d30)
#'
#' # a function of your data:
#' ffplot(range(price) ~ color, d30)
#'
#' # confidence intervals
#' ffplot(ci(price, 0.95) ~ color, d30)
#'
#' # multiple plots:
#' ffplot(price + ci(price) ~ color, d30)
#'
#' # choosing geoms:
#' ffplot(line(mean(price)) + ci(price, .99) ~ color, d30)
#' ffplot(violin(price) ~ color, diamonds)
#'
#' # adding options:
#' ffplot(price, diamonds, geom = "point", shape = 3)
#'
#' # adding options per y variable:
#' ffplot(point(price, alpha = 0.2, color = "red") + smooth(price, color = "orange", size = 2, se = TRUE) ~ carat, diamonds)
#'
#' \dontrun{
#' # with dplyr
#' library(dplyr)
#' diamonds %>% ffplot(cut ~ color)
#'
#' }
#' # Facetting:
#' ffplot(price ~ carat | color, diamonds)
#' ffplot(smooth(mean(price)) ~ carat | color + cut, diamonds)
ffplot.formula <- function(formula, data = parent.frame(), ..., subset = NULL, smooth = NULL) {
  fml <- formula
  lhs_all <- attr(terms(fml[-3], keep.order = TRUE), "term.labels")
  rhs <- attr(terms(Formula(fml[-2])), "term.labels")
  xlab. <- rhs
  if (length(rhs) > 1) {
    if (as.character(fml[[3]][[1]]) != "|") warning("Facetting on right hand side variables.
      Use `|` to separate facets, like: y ~ x | facet1 + facet2.
      Otherwise, behaviour may change in future versions.")
    facet <- rhs[-1]
    xlab. <- rhs[1]
    rhs <- paste(rhs, collapse = " + ")
    facet <- if (length(facet) > 1) facet1 ~ facet2 else ~ facet1
  }
  ylab. <- character(0)

  ggp <- ggplot()

  for (lhs in lhs_all) {
    geom_name <- NULL
    geom_args <- list()
    outer_fun <- parse(text = lhs)[[1]] # this gets e.g. "log(y)" in "log(y) + ..."
    if (is.call(outer_fun)) {
      outer_fun_name <- as.character(outer_fun[[1]])
      try(geom_name <- match.arg(outer_fun_name, geom_names), silent = TRUE)
      if (! is.na(geom_name)) {
        lhs <- deparse(outer_fun[[2]])
        geom_name <- paste0("geom_", geom_name)
        if (length(outer_fun) > 2) geom_args <- as.list(outer_fun[-(1:2)])
      } else if (outer_fun_name %in% names(geom_map)) {
        geom_name <- geom_map[[outer_fun_name]]
      }
    }
    # if we still haven't got geom_name, we'll decide by the mode of lhs and rhs

    fml <- formula(paste(lhs, "~", rhs))
    result <- fave(fml, data, subset) # gives a data frame, unique vals of LHS for each RHS. Last col can be matrix, list
    y <- result[, ncol(result)]
    lenresult <- if (is.matrix(y)) rep(ncol(y), nrow(y)) else if (is.list(y)) sapply(y, length) else rep(1, length(y))
    if (is.list(y)) y <- unlist(y)
    if (is.matrix(y)) y <- c(t(y))
    dfr <- result[rep(1:nrow(result), lenresult),]
    names(dfr)[1] <- "x"      # First column is always the x. Others (if any) are for facetting
    names(dfr)[2:ncol(dfr)] <- paste("facet", 1:(ncol(result)-1), sep = "") # TODO: reuse actual names?
    dfr$y <- y
    dfr$count <- unlist(lapply(lenresult, seq_len))
    if (is.null(geom_name)) geom_name <-
      if (! is.numeric(dfr$y)) ifelse(is.numeric(dfr$x), "geom_density", "geom_histogram") else
      if (all(lenresult == 2)) "geom_linerange" else "geom_point"
    # by here geom_name is defined

    dots <- list(...)
    dot_args <- dots[sapply(dots, class) != "uneval"]
    dflt_geom_args <- list(data = dfr)
    if (geom_name %in% names(extra_args)) dflt_geom_args <- merge(extra_args[[geom_name]], dflt_geom_args)
    dflt_geom_args <- merge(dot_args, dflt_geom_args)
    geom_args <- merge(geom_args, dflt_geom_args)

    mapping <- list(x = "x", y = "y")
    if (geom_name %in% names(extra_mapping)) mapping <- merge(extra_mapping[[geom_name]], mapping)
    #mapping <- mapping[! sapply(mapping, is.null)] # delete some elements
    dot_mapping <- dots[sapply(dots, class) == "uneval"]
    mapping <- modifyList(mapping, dot_mapping)
    geom_args$mapping <- do.call(aes_string, mapping)

    lyr <- do.call(geom_name, geom_args)
    ggp <- ggp + lyr
    if (geom_name %in% c("geom_histogram", "geom_density", "geom_freqpoly")) ggp <- ggp +
      guides(fill = guide_legend(title = lhs))
    ylab. <- c(ylab., lhs)
  }
  ggp <- ggp + xlab(xlab.) + ylab(paste(ylab., collapse = " + "))
  if (exists("facet", inherits = FALSE)) ggp <- ggp + facet_grid(facet)
  return(ggp)
}

#' @export
ffplot <- function (x, ...) UseMethod("ffplot")


#' @rdname ffplot
#' @export
ffplot.default <- function (data, ...) ffplot.formula(..., data = data)
