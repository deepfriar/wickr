#' @describeIn sumer clm
#' @export
sumer.clm <- function(x, ...) {sumer.default(x, ...)}

#' @describeIn tidify clm
#' @export
tidify.clm <- function(x, ...) {broom::tidy(lmtest::coeftest(x))}

#' @describeIn ascribe clm
#' @export
ascribe.clm <- function(x, ...) {
  z <- as.list(plyr::each(stats::df.residual, stats::AIC, stats::BIC)(x))

  z$link <- x$link # icky use of object subsetting because there's not an accessor function available
  z$n    <- stats::nobs(x)

  z
}

#' @describeIn kind clm
#' @export
kind.clm <- function(x, ...) {"ordinal"}
