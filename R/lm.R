#' @describeIn sumer OLS
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
sumer.lm <- function(x, vcov.=NULL, ...) {sumer.default(x, vcov.=vcov., vcov_name=deparse(substitute(vcov.)), ...)}

#' @describeIn tidify uses \code{lmtest::coeftest} with chosen covariance matrix.
#' @param vcov. a specification of the covariance matrix. See \code{\link[lmtest]{coeftest}} for details.
#' @export
tidify.lm <- function(x, vcov.=NULL, ...) {broom::tidy(lmtest::coeftest(x, vcov.=vcov.))}

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

  z
}

#' @describeIn kind "OLS"
#' @export
kind.lm <- function(x, ...) {"OLS"}
