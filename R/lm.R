#' @describeIn sumer OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
sumer.lm <- function(x, vcov.=NULL, ...) {
  y <- tidify(x, vcov.=vcov.)

  attributes(y)$sumer <- ascribe(x, vcov.=vcov.)

  attributes(y)$sumer$kind <- kind(x)

  class(y) <- c("sumer", class(y))

  y
}

#' @describeIn tidify OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
tidify.lm <- function(x, vcov.=NULL, ...) {broom::tidy(lmtest::coeftest(x, vcov.=vcov.))}

#' @describeIn ascribe OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
ascribe.lm <- function(x, vcov.=NULL, ...) {
  z <- as.list(broom::glance(x))

  z$vcov <- vcov.

  z
}

#' @describeIn kind OLS
#' @export
kind.default <- function(x, ...) {"OLS"}
