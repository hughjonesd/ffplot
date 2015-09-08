
library(ggplot2)
library(reshape2)
#library(formula.tools)

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

ci <- function(x, level = 0.95, na.rm = TRUE, df = Inf) {
  if (na.rm) x <- na.omit(x)
  lx <- length(x)
  se <- sd(x)/sqrt(lx)
  mean(x) + c(- se, + se) * qt((1 + level)/2, df)
}

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
    # what do I want?
    # mean(y) ~ x : single number
    # range(y), ci(y) : two numbers
    # table(y) : multiple numbers with names, all same size
    # y ~ x : multiple numbers
    # suppose if y is a factor or a character, I automatically choose a barplot of counts
    # then I could just do y ~ x



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
    lyr <- switch(match.arg(geom_name, geom_names),
      "point"     = geom_point(aes(x = x, y = y), data = dfr, size = 3, ...),
      "line"      = geom_line(aes(x = x, y = y, group = 1), data = dfr, ...),
      "boxplot"   = geom_boxplot(aes(x = factor(x), y = y), data = dfr, ...),
      "barplot"   = geom_bar(aes(x = x, y = y), stat = "identity", data = dfr, ...),
      "histogram" = geom_histogram(aes(x = x, group = y, fill = y), data = dfr, position = "fill"),
      "violin"    = geom_violin(aes(x = factor(x), y = y), data = dfr, ...),
      "errorbar"  = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
        geom = "errorbar", width = 0.25, data = dfr, ...),
      "linerange" = stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, mapping = aes(x = x, y = y, group = x),
        geom = "linerange", data = dfr, ...)
    )
    ggp <- ggp + lyr
  }
  ggp <- ggp + xlab(rhs) + ylab(as.character(formula[[2]]))
  return(ggp)
}

if (FALSE) {
data("diamonds", package = "ggplot2")
d2 <- diamonds[1:15,]
# basic use
ffplot(price ~ color, d2)
ffplot(price ~ carat, d2)

# factor on LHS
ffplot(color ~ cut, d2)
ffplot(color ~ cut, d2, position = "fill")

# manual geoms
ffplot(price ~ color, diamonds, geom ="box")
ffplot(price ~ color, diamonds, geom ="violin")

# functions on left and right
ffplot(range(price) ~ color, d2)
ffplot(range(price) ~ color, d2, geom = "errorbar")
ffplot(carat ~ cut(price, c(0, 335, 400)), diamonds)
ffplot(mean(price) ~ color, d2, geom = "point")
ffplot(mean(price) ~ color, d2, geom = "point", shape = 3)

# multiple LHS
ffplot(price + quantile(price, c(0.25, 0.75)) ~ color, d2)
ffplot(price + mean(price) ~ color, d2)
# TODO make bars not black!
ffplot(price + mean(price) ~ color, d2, geom = c("point", "bar"))
ffplot(price + mean(price) ~ color, diamonds, geom = c("violin", "line"))
ffplot(ci(price) ~ color, diamonds) # errorbar default
ffplot(mean(price) + ci(price) ~ color, diamonds)

# without data argument
color <- d2$color
price <- d2$price
ffplot(price ~ color)
rm(color, price)

# smoothing
ffplot(mean(price) ~ carat, d2) # do we smooth or what?
} # if (FALSE)
