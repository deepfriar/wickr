# @param .depth integer. Levels from top of nested list of objects. Default \code{0}.
# @param .names character. Vector of names to represent levels, from the top downward. Default \code{NULL}.
#' @describeIn sumer Unrolls lists of objects to be tidily summarized.
#' @export
sumer.list <- function(x,
                       margins=FALSE,
                       ...) {

  ## don't let this operate on arbitrary objects that are technically also lists
  if(!methods::is(x, "list")) {return(NextMethod("sumer", margins=margins, ...))}

  sumer(dplyr::tibble(x = x), margins = margins, ...)
}

# @describeIn unlevel returns nothing
# @export
# unlevel.list <- function(x, ...) {data.frame(term=character(0), Lterm=character(0))}
