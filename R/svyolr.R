#' @describeIn sumer svyolr
#' @export
sumer.svyolr <- function(x, ...) {sumer.default(x, ...)}

#' @describeIn tidify svyolr
#' @export
tidify.svyolr <- function(x, ...) {broom::tidy(lmtest::coeftest(x, vcov=survey:::vcov.svyolr))} # augh!

#' @describeIn ascribe svyolr
#' @export
ascribe.svyolr <- function(x, ...) {
  z <- as.list(plyr::each(stats::df.residual, stats::deviance)(x))

  # icky use of object subsetting because there's not an accessor function available
  z$link <- stringr::str_replace(x$method, "logistic", "logit")
  z$n    <- stats::nobs(x)
  z
}

#' @describeIn kind svyolr
#' @export
kind.svyolr <- function(x, ...) {"ordinal"}
