#' @describeIn sumer The method for otherwise unhandled objects.
#' @export
sumer.default <- function(x, ...) {
  y <- tidify(x, ...)

  z <- unlevel(x)

  y <- plyr::join(y, z, "term")

  y$term <- ifelse(is.na(y$Lterm), as.character(y$term), as.character(y$Lterm))

  y <- y[, c(setdiff(colnames(y),   c("estimate", "std.error", "statistic", "p.value", "Lterm")),
             intersect(colnames(y), c("estimate", "std.error", "statistic", "p.value")))]

  attributes(y)$sumer <- ascribe(x, ...)

  attributes(y)$sumer$kind <- kind(x, ...)

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

  y$n <- stats::nobs(x)

  y
}

#' @describeIn kind The method for otherwise unhandled objects.
#' @export
kind.default <- function(x, ...) {class(x)}

#' @describeIn unlevel The method for otherwise unhandled objects.
#' @export
unlevel.default <- function(x, ...) {
  w <- reshape2::melt(x$xlevels, value.name="level", level="term")

  w$term <- paste0(w$Lterm, w$level)

  as.data.frame(w)
}
