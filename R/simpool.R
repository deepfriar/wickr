#' @describeIn tidify uses coeftest
#' @export
tidify.simpool <- function(x, ...) {
  stop("simpool is hard-deprecated, rolling my own imputation wrappers isn't ever going to happen")

  y <- data.frame(term      = names(x$coefficients),
                  estimate  = x$coefficients,
                  std.error = sqrt(diag(x$vcov)))

  y$statistic <- y$estimate / y$std.error
  y$p.value   <- pmin(stats::pnorm(y$statistic), 1 - stats::pnorm(y$statistic))

  y$component <- stringr::str_extract(y$term, "^\\(.+\\)_")
  y$component <- ifelse(is.na(y$component), "(location)_", y$component)
  y$term      <- stringr::str_replace(y$term, stringr::fixed(y$component), "")
  y$component <- stringr::str_extract(y$component, "\\w+")

  y[, c("component", setdiff(colnames(y), "component"))]
}

#' @describeIn ascribe whatever's there
#' @export
ascribe.simpool <- function(x, ...) {
  Z <- c("family", "deviance", "aic", "null.deviance", "df.residual", "df.null", "n", "nobs")
  Z <- intersect(names(x), Z)

  names(Z) <- Z

  z <- x[Z]

  z$nobs <- if(is.null(z$nobs)) {tryCatch(stats::nobs(x), error=function(e) {})} else (z$nobs)
  z$n    <- if(is.null(z$n)) {z$nobs} else {z$n}

  z
}

#' @describeIn unlevel OK then
#' @export
unlevel.simpool <- function(x, ...) {NextMethod("simpool", x, ...)}

#' @describeIn kind OK then
#' @export
kind.simpool <- function(x, ...) {paste(class(x)[[2]], "(pooled)")}
