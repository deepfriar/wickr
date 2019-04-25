#' @describeIn sumer simulation results from \code{gilez}, which are always tall
#' @param h_0 numeric value of the pointlike null hypothesis. Default \code{0}.
#' @export
sumer.gilez <- function(x, margins=FALSE, h_0=0, ...) {
  if(margins) {warning("The margins argument doesn't apply to substantive effects already simulated and is ignored.")}

  w <- setdiff(colnames(x), c(".id", "value"))

  y <- plyr::ddply(x, w, function(u) {
    z <- u$value

    S <- c(estimate  = mean(z),
           stats::quantile(z, c(lower=0.025, upper=0.975)))

    S["p.value"] <- 2 * min(mean(z < h_0), mean(z > h_0)) # TODO: what if mean is on the wrong side of the null

    S
  })

  attributes(y)$sumer <- attr(x, "sumer") # resolved: a gilez object should have a sumer attribute

  class(y) <- c("sumer", class(y))

  y
}
