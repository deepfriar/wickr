#' @describeIn sumer clm
#' @export
sumer.clm <- function(x, margins=FALSE, ...) {sumer.default(x, margins=margins, ...)} # y tho

#' @describeIn tidify uses coeftest
#' @export
tidify.clm <- function(x, margins=FALSE, ...) {
  if(margins) {stop("The margins package doesn't support ordinal models of class clm from package ordinal.")}

  broom::tidy(lmtest::coeftest(x))
}

#' @describeIn ascribe residual df, AIC, BIC, link, and n
#' @export
ascribe.clm <- function(x, ...) {
  z <- as.list(plyr::each(df.residual=stats::df.residual, AIC=stats::AIC, BIC=stats::BIC, n=stats::nobs)(x))

  z$link <- x$link # icky use of object subsetting because there's not an accessor function available

  z
}

#' @describeIn kind "ordinal"
#' @export
kind.clm <- function(x, ...) {"ordinal"}
