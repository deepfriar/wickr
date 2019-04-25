#' @describeIn tidify AMCE objects from package cjoint
#' @param .sdiff logical. Second differences? Default \code{FALSE}. Currently ignored, though.
#' @export
tidify.amce <- function(x, margins=FALSE, .sdiff=FALSE, ...) {
  if(margins) {warning("The margins argument doesn't apply to AMCE objects and is ignored.")}

  foo <- Reduce(cbind, x$cond.estimates)
  bar <- Reduce(cbind, x$estimates)

  foo[, colnames(bar)] <- bar # egad this is inelegant

  foo <- reshape2::dcast(reshape2::melt(foo), Var2 ~ Var1)

  colnames(foo) <- c("term", "estimate", "std.error")

  foo$outcome <- ifelse(stringr::str_detect(foo$term, x$respondent.varying), "Difference", "Average Effect")
  foo$term <- qdap::multigsub(paste0("^(.*:)?", foo$term[foo$outcome=="Average Effect"], "(:.*)?$"),
                              foo$term[foo$outcome=="Average Effect"], foo$term, fixed=FALSE)

  foo$level <- unlist(x$user.levels)[as.character(foo$term)] # ffs
  foo$term  <- qdap::multigsub(qdap::multigsub(names(x$user.names),  "", foo$term), "", foo$term)

  foo$level[stringr::str_detect(foo$term, x$respondent.varying)] <- "(Intercept)"

  ## the conditional differences

  ## the main AMCEs
  # bar <- summary(x)
  # colnames(bar$amce) <- c("term", "level", "estimate", "std.error", "statistic", "p.value", " ")

  # baz <- plyr::ldply(list(Main="estimates", Diff="cond.estimates"), .amce_tidify, x=x, .id="component")

  foo$statistic <- foo$estimate / foo$std.error
  foo$p.value   <- 2 * stats::pnorm(abs(foo$statistic), lower.tail=FALSE) # package cjoint prescribes pnorm, not pt

  foo[, c("outcome", "term", "level", setdiff(colnames(foo), c("outcome", "term", "level")))]
}

# .amce_tidify <- function(x, i) {
#   foo <- reshape2::dcast(plyr::ldply(x[[i]], reshape2::melt, variable="term"), Var2 ~ Var1)
#   foo <- if(any(stringr::str_detect(foo$Var2, ":"))) {foo[stringr::str_detect(foo$Var2, ":"), ]} else {foo}

  # colnames(foo) <- c("term", "estimate", "std.error")

  # foo$term[stringr::str_detect(foo$term, ":")]  <-
#     qdap::multigsub(
      # qdap::multigsub(names(x$user.levels)[!stringr::str_detect(names(x$user.levels), x$respondent.varying)],
                      # "",
                      # foo$term[stringr::str_detect(foo$term, ":")]),
      # "",
      # foo$term[stringr::str_detect(foo$term, ":")])

  # foo
# }

#' @describeIn ascribe the only
#' @export
ascribe.amce <- function(x, ...) {list(n=x$numrespondents)}

#' @describeIn unlevel returns NULL; this step is handled in the \code{\link{tidify}} method
#' @export
unlevel.amce <- function(x, ...) {NULL}

#' @describeIn delevel returns y; this step is handled in the \code{\link{tidify}} method
#' @export
delevel.amce <- function(x, y, z, ...) {y}
