#' @describeIn sumer GLM
#' @export
sumer.glm <- function(x, margins=FALSE, ...) {sumer.default(x, margins=margins, ...)}

# TODO: why tidify.default(x, ...) and not NextMethod("tidify", x, ...) ? # why any function?
#' @describeIn tidify GLM
#' @export
tidify.glm <- function(x, margins=FALSE, ...) {tidify.default(x, margins=margins, ...)}

#' @describeIn ascribe GLM
#' @export
ascribe.glm <- function(x, ...) {
  z <- ascribe.default(x)

  z$link <- stats::family(x)$link

  z
}

#' @describeIn kind GLM
#' @export
kind.glm <- function(x, ...) {stats::family(x)$family}
