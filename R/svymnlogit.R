#' @describeIn sumer svymnlogit
#' @export
sumer.svymnlogit <- function(x, ...) {sumer.default(x, ...)}

#' @describeIn tidify svymnlogit
#' @export
tidify.svymnlogit <- function(x, ...) {summary(x)}

#' @describeIn ascribe svymnlogit
#' @export
ascribe.svymnlogit <- function(x, ...) {as.list(link="logit")}

#' @describeIn kind svymnlogit
#' @export
kind.svymnlogit <- function(x, ...) {"multinomial"}
