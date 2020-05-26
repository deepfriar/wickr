# @param .depth integer. Levels from top of nested list of objects. Default \code{0}.
# @param .names character. Vector of names to represent levels, from the top downward. Default \code{NULL}.
#' @describeIn sumer Unrolls lists of objects to be tidily summarized.
#' @export
sumer.list <- function(x,
                       margins=FALSE,
                       # .depth=0,
                       # .names=NULL,
                       ...) {
  ## don't let this sucker operate on arbitrary objects that are technically also lists
  if(!methods::is(x, "list")) {return(NextMethod("sumer", margins=margins, ...))}

  sumer(dplyr::tibble(x = x), margins = margins, ...)

  # ## sumer each element
  # ## this will apply sumer.list recursively to elements that are lists
  # x <- lapply(x, sumer, .depth = .depth + 1, .names = .names[-1], margins=margins, ...)
  #
  # ## Pick the name for the current level of depth out of the list
  # ## If there isn't a name, call it "Level N"
  # name <- c(.names[1], paste("Level", .depth))[1]
  #
  # ## make sure every element of the list is named
  # X <- names(x)
  # X <- if(is.null(X)) {1:length(x)} else {X}
  # names(x) <- ifelse(X=="", 1:length(x), X)
  #
  # ## stack the metadata tables together
  # ## TODO: later consider making sure the sumer attribute is a data.frame to start with
  # z <- plyr::ldply(lapply(lapply(x, attr, which="sumer"), data.frame, check.names=FALSE), .id=name)
  #
  # ## stack the tidy summaries together
  # y <- plyr::ldply(x, .id=name)
  #
  # attributes(y)$sumer <- z
  #
  # class(y) <- c("sumer", class(y))
  #
  # y
}

# @describeIn unlevel returns nothing
# @export
# unlevel.list <- function(x, ...) {data.frame(term=character(0), Lterm=character(0))}
