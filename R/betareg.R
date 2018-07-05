#' @describeIn tidify uses coeftest
#' @export
tidify.betareg <- function(x, ...) {
  y <- broom::tidy(lmtest::coeftest(x))

  y$component <- stringr::str_extract(y$term, "^\\(.+\\)_")
  y$component <- ifelse(is.na(y$component), "(location)_", y$component)
  y$term      <- stringr::str_replace(y$term, stringr::fixed(y$component), "")
  y$component <- stringr::str_extract(y$component, "\\w+")

  y[, c("component", setdiff(colnames(y), "component"))]
}

#' @describeIn ascribe residual df, AIC, BIC, link, and n
#' @export
ascribe.betareg <- function(x, ...) {
  z <- as.list(plyr::each(df.residual=stats::df.residual, AIC=stats::AIC, BIC=stats::BIC, n=stats::nobs)(x))

  z$link         <- x$link$mean$name
  z$`link (phi)` <- x$link$precision$name

  z
}

#' @describeIn unlevel \code{levels$full} replaces \code{xlevels}
#' @export
unlevel.betareg <- function(x, ...) {
  w <- reshape2::melt(x$levels$full, value.name="level", level="term")

  w$term <- paste0(w$Lterm, w$level)
  w$term <- stringr::str_replace(w$term, stringr::fixed("(phi)_"), "phi | ")

  as.data.frame(w)
}
