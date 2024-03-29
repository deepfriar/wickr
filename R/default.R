#' @describeIn sumer The method for otherwise unhandled objects.
#' @export
sumer.default <- function(x, margins=FALSE, ...) {
  y <- tidify(x, margins=margins, ...) # the main step

  y <- encomp(x, y)

  z <- unlevel(x)       # extract levels of factor covariates

  y <- delevel(x, y, z) # attach  levels of factor covariates

  attributes(y)$sumer <- ascribe(x, ...)   # staple the metadata on there

  attributes(y)$sumer$kind <- kind(x, ...) # what kind of model is it?

  class(y) <- c("sumer", class(y))

  y
}

#' @describeIn encomp Do nothing to otherwise unhandled objects
#' @export
encomp.default <- function(x, y, ...) {y}

#' @describeIn tidify The method for otherwise unhandled objects.
#' @export
tidify.default <- function(x, margins=FALSE, ...) {
  if(margins) {
    y <- summary(margins::margins(x, margins=margins, ...)) # sic

    ## will this do?
    y <- `colnames<-`(y, c("term", "estimate", "std.error", "statistic", "p.value", "lower", "upper"))

    y[, 1:5]
  } else {
    broom::tidy(lmtest::coeftest(x, ...))
  }
}

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
  w <- x$xlevels

  if(length(w)) {
    w <- purrr::map(w, list)
    w <- dplyr::as_tibble(w)
    w <- tidyr::pivot_longer(w, tidyselect::everything(), "Lterm", values_to = "level")
    w <- tidyr::unnest(w, .data$level)
    w <- dplyr::mutate(w, term = paste0(.data$Lterm, .data$level))

    # w <- reshape2::melt(as.list(x$xlevels), value.name="level", level="term")

    # w$term <- paste0(w$Lterm, w$level)

    as.data.frame(w)
  } else {
    data.frame(level = character(0), Lterm = character(0), term = character(0))
  }
}

#' @describeIn delevel The method for otherwise unhandled objects.
#' @export
delevel.default <- function(x, y, z, ...) {
  y$level <- qdap::multigsub(as.character(z$term), as.character(z$level), y$term)
  y$term  <- qdap::multigsub(as.character(z$term), as.character(z$Lterm), y$term)

  y$level[y$level==y$term] <- NA

  y[, c(setdiff(colnames(y),   c("estimate", "std.error", "statistic", "p.value", "Lterm")),
        intersect(colnames(y), c("estimate", "std.error", "statistic", "p.value")))]
}
