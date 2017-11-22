#' @describeIn sumer The method for otherwise unhandled objects.
#' @export
sumer.default <- function(x, ...) {
  y <- tidify(x)

  attributes(y)$sumer <- ascribe(x)

  attributes(y)$sumer$kind <- kind(x)

  class(y) <- c("sumer", class(y))

  y
}

#' @describeIn tidify The method for otherwise unhandled objects.
#' @export
tidify.default <- function(x, ...) {broom::tidy(x)}

#' @describeIn ascribe The method for otherwise unhandled objects.
#' @export
ascribe.default <- function(x, ...) {
  y <- as.list(broom::glance(x))

  y$n <- stats::nobs(y)

  y
}

#' @describeIn kind The method for otherwise unhandled objects.
#' @export
kind.default <- function(x, ...) {class(x)}
