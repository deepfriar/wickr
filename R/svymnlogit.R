#' @describeIn sumer svymnlogit
#' @export
sumer.svymnlogit <- function(x, ...) {
  if(requireNamespace("svymnlogit")) {return(NextMethod("sumer"))}

  stop("You must install the svymnlogit package to work with svymnlogit objects.")
}

#' @describeIn tidify svymnlogit
#' @export
tidify.svymnlogit <- function(x, ...) {
  if(requireNamespace("svymnlogit")) {return(summary(x))}

  stop("You must install the svymnlogit package to work with svymnlogit objects.")
}

#' @describeIn ascribe only the link function ("logit")
#' @export
ascribe.svymnlogit <- function(x, ...) {
  if(!requireNamespace("svymnlogit")) {stop("You must install the svymnlogit package.")}

  z <- as.list(plyr::each(df.residual=stats::df.residual, deviance=stats::deviance)(x))

  z$link <- "logit"
  z$n    <- nrow(stats::fitted(x)) # TODO: why isn't the nobs method available from svymnlogit

  z
}

#' @describeIn kind "multinomial"
#' @export
kind.svymnlogit <- function(x, ...) {"multinomial"}

## TODO: nah, handle the xlevels in the svymnlogit function; they'll be needed at some point
#' @describeIn unlevel svymnlogit
#' @export
unlevel.svymnlogit <- function(x, ...) {
  if(!requireNamespace("svymnlogit")) {stop("You must install the svymnlogit package.")}

  f <- stats::model.frame(x)

  u <- all.vars(stats::formula(x))[-1]

  names(u) <- u

  v <- plyr::llply(plyr::llply(u, getElement, object=f), levels)

  unlevel(list(xlevels=v[sapply(v, length) > 0]))
}
