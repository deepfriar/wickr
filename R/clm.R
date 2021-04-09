#' @describeIn sumer clm
#' @export
sumer.clm <- function(x, margins=FALSE, ...) {sumer.default(x, margins=margins, ...)} # y tho

#' @describeIn tidify uses coeftest
#' @export
tidify.clm <- function(x, margins=FALSE, ...) {
  if(margins) {stop("The margins package doesn't support ordinal models of class clm from package ordinal.")}

  broom::tidy(lmtest::coeftest(x, vcov. = ordinal:::vcov.clm(x)))
}

# TODO: `encomp.clm` (because `ordinal::clm` provides separate location and scale formula args)

#' @describeIn ascribe residual df, AIC, BIC, link, and n
#' @export
ascribe.clm <- function(x, ...) {
  z <- list(df.residual=stats::df.residual, AIC=stats::AIC, BIC=stats::BIC, n=stats::nobs)
  z <- lapply(z, function(i) {i(x)})

  z$link <- x$link # icky use of object subsetting because there's not an accessor function available

  z
}

#' @describeIn kind "ordinal"
#' @export
kind.clm <- function(x, ...) {"ordinal"}
