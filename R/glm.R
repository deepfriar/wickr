#' @describeIn sumer GLM
#' @export
sumer.glm <- function(x, ...) {sumer.default(x, ...)}

#' @describeIn tidify GLM
#' @export
tidify.glm <- function(x, ...) {tidify.default(x, ...)}

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
