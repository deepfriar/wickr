#' @describeIn tidify uses coeftest
#' @export
tidify.crch <- function(x, ...) {
  y <- broom::tidy(lmtest::coeftest(x))

  y$component <- stringr::str_extract(y$term, "^\\(.+\\)_")
  y$component <- ifelse(is.na(y$component), "(location)_", y$component)
  y$term      <- stringr::str_replace(y$term, stringr::fixed(y$component), "")
  y$component <- stringr::str_extract(y$component, "\\w+")

  y[, c("component", setdiff(colnames(y), "component"))]
}

#' @describeIn ascribe AIC, BIC, n, and link
#' @export
ascribe.crch <- function(x, ...) {
  z <- as.list(plyr::each(AIC=stats::AIC, BIC=stats::BIC, n=stats::nobs)(x))

  z$`link (scale)` <- x$link$scale$name

  z
}

#' @describeIn kind "censored"
#' @export
kind.crch <- function(x, ...) {"censored"}

#' @describeIn unlevel \code{levels$full} replaces \code{xlevels}
#' @export
unlevel.crch <- function(x, ...) {
  w <- reshape2::melt(x$levels$full, value.name="level", level="term")

  w$term <- paste0(w$Lterm, w$level)
  w$term <- stringr::str_replace(w$term, stringr::fixed("(scale)_"), "scale | ")

  as.data.frame(w)
}
