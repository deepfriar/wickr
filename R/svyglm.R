#' @describeIn ascribe survey GLM
#' @export
ascribe.svyglm <- function(x, ...) {
  if(!requireNamespace("survey")) {stop("You must install the survey package to work with svyglm objects.")}

  # stats::AIC appears to sometimes fail if model matrix is rank deficient
  z <- as.list(plyr::each(df.residual = stats::df.residual,
                          deviance    = stats::deviance,
                          n           = stats::nobs,
                          logLik      = stats::logLik)(x))

  z$link <- stats::family(x)$link

  z
}
