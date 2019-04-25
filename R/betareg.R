#' @describeIn tidify \code{margins} plays strangely with \code{betareg} objects; this method works around those quirks
#' @param set data.frame. Where is the data located? Defaults to data within the model object.
#' @param variables character. Which terms to get effects for. Default \code{NULL} (\[everyone.gif\]).
#' @export
tidify.betareg <- function(x, margins=FALSE, set=if(is.null(x$model)) {margins::find_data(x)} else {x$model}, variables=NULL, ...) {
  variables <- if(is.null(variables)) {attr(stats::terms(x), "term.labels")} else {variables}

  # NextMethod("tidify", x, margins=margins, variables=variables, ...)
  # does NextMethod fail to pass args not named in the next method?
  tidify.default(x, margins=margins, data=set, variables=variables, ...)
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
