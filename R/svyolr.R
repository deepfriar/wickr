#' @describeIn sumer svyolr
#' @export
sumer.svyolr <- function(x, margins=FALSE, ...) {sumer.default(x, margins=margins, ...)}

#' @describeIn tidify reaches into \code{survey} to get the right \code{vcov} for use in \code{lmtest::coeftest}
#' @export
tidify.svyolr <- function(x, margins=margins, ...) {
  # require the survey namespace because it contains the correct vcov method
  if(!requireNamespace("survey")) {stop("You must install the survey package to work with svyolr objects.")}
  if(margins) {stop("The margins package doesn't support ordinal models of class svyolr from package survey.")}

  # for unknown reasons, may return the wrong thing the first time it is called
  k <- broom::tidy(lmtest::coeftest(x))

  broom::tidy(lmtest::coeftest(x))
}

#' @describeIn ascribe residual df, deviance, link, and n
#' @export
ascribe.svyolr <- function(x, ...) {
  # require the survey namespace because it contains the correct accessor methods
  if(!requireNamespace("survey")) {stop("You must install the survey package to work with svyolr objects.")}

  z <- as.list(plyr::each(df.residual=stats::df.residual, deviance=stats::deviance)(x))

  # icky use of object subsetting because there's not an accessor function available
  z$link <- stringr::str_replace(x$method, "logistic", "logit")
  z$n    <- nrow(stats::fitted(x))

  z
}

#' @describeIn kind "ordinal"
#' @export
kind.svyolr <- function(x, ...) {"ordinal"}
