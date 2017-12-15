#' @describeIn sumer simulation results from \code{gilez}, which are always tall
#' @export
sumer.gilez <- function(x, ...) {
  w <- setdiff(colnames(x), c(".id", "value"))

  y <- plyr::ddply(x, w, function(u) {
    z <- u$value

    S <- c(estimate  = mean(z),
           stats::quantile(z, c(lower=0.025, upper=0.975)))

    S["p.value"] <- 2 * min(mean(z < 0), mean(z > 0)) # TODO: what if mean is on the wrong side of zero

    S
  })

  attributes(y)$sumer <- attr(x, "sumer") # resolved: a gilez object should have a sumer attribute

  class(y) <- c("sumer", class(y))

  y
}
