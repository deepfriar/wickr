#' @describeIn sumer OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
sumer.lm <- function(x, margins=FALSE, vcov.=stats::vcov, ...) {
  sumer.default(x, vcov.=vcov., vcov_name=deparse(substitute(vcov.)), margins=margins, ...)
}

#' @describeIn tidify uses \code{lmtest::coeftest} with chosen covariance matrix.
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
tidify.lm <- function(x, margins=FALSE, vcov.=stats::vcov, variables=NULL, ...) {
  if(margins) {
    varlabels <- attr(stats::terms(x), "term.labels")
    variables <- if(is.null(variables)) {varlabels} else {intersect(variables, varlabels)}
    variables <- variables[!stringr::str_detect(variables, ":")]

    vcm <- if(methods::is(vcov., "function")) {vcov.(x, ...)} else {vcov.}

    NextMethod("tidify", x, margins=margins, vcov=vcm, data=if(is.null(x$model)) {margins::find_data(x)} else {x$model}, variables=variables, ...)
  } else {
    broom::tidy(lmtest::coeftest(x, vcov.=vcov., ...), ...)
  }
}

#' @describeIn ascribe OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @param vcov_name character. A name to call the covariance matrix. Default \code{deparse(substitute(vcov.))}.
#' @export
ascribe.lm <- function(x, vcov.=NULL, vcov_name=deparse(substitute(vcov.)), ...) { # will this work?
  z <- as.list(broom::glance(x))

  z$vcov <- if(is.function(vcov.)) {
    stringr::str_replace(vcov_name, ".*:+", "")
  } else if(is.matrix(vcov.)) {"custom"} else {"OLS"}

  z$n <- stats::nobs(x)

  Z <- summary(x)
  z$r.squared     <- Z$r.squared
  z$adj.r.squared <- Z$adj.r.squared
  z$sigma         <- Z$sigma
  # z$fstatistic    <- Z$fstatistic # NO DONT

  z
}

#' @describeIn kind "OLS"
#' @export
kind.lm <- function(x, ...) {"OLS"}
