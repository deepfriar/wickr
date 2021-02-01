#### helper functions ----
unpack <- function(foo, outcome) {
  foo <- dplyr::as_tibble(purrr::map(foo, list))
  foo <- dplyr::mutate(foo, dplyr::across(tidyselect::everything(), function(y) {
    list(dplyr::as_tibble(t(dplyr::first(y)), rownames = "Level"))
  }))
  foo <- tidyr::pivot_longer(foo, tidyselect::everything(), "term")
  foo <- tidyr::unnest(foo, .data$value)
  foo <- dplyr::group_by(foo, .data$term, .data$Level)
  foo <- dplyr::group_modify(foo, function(.x, .y) {
    dplyr::rename_with(.x, function(y) {c("estimate", "std.error")[1:length(y)]})
  })
  foo <- dplyr::mutate(foo, outcome = outcome)

  foo
}

depack <- function(foo, baz, name, value) {
  baz <- dplyr::as_tibble(baz)
  baz <- tidyr::pivot_longer(baz, tidyselect::everything(), name, values_to = value)

  dplyr::left_join(foo, baz)
}

#### ----
#' @describeIn tidify AMCE objects from package cjoint
#' @export
tidify.amce <- function(x, margins=FALSE, ...) {
  if(margins) {warning("The margins argument doesn't apply to AMCE objects and is ignored.")}

  foo <- unpack(x$estimates,      "Average Effect")
  bar <- unpack(x$cond.estimates, "Difference")
  bar <- dplyr::anti_join(bar, foo, c("term", "Level"))

  foo <- dplyr::bind_rows(foo, bar)

  foo <- dplyr::group_by(foo)
  foo <- dplyr::mutate(foo, dplyr::across(c("term", "Level"), stringr::str_remove, pattern = ":.*$"))

  foo <- depack(foo, x$user.levels, "Level",    "level")

  baz <- dplyr::as_tibble(x$baselines)
  baz <- tidyr::pivot_longer(baz, tidyselect::everything(), "term", values_to = "Baseline")
  baz <- dplyr::mutate(baz, Baseline = paste0(.data$term, .data$Baseline))
  foo <- dplyr::left_join(foo, baz)

  foo <- depack(foo, x$user.levels, "Baseline", "baseline")

  foo <- dplyr::mutate(foo,
                       statistic = .data$estimate / .data$std.error,
                       p.value   = 2 * stats::pnorm(abs(.data$statistic), lower.tail=FALSE))

  foo <- dplyr::select(foo,
                       .data$outcome,
                       .data$term,
                       .data$level,
                       .data$baseline,
                       tidyselect:::where(is.numeric))

  foo <- dplyr::mutate(foo, term = stringr::str_replace(.data$term, paste0("^", x$respondent.varying, "$"), "(Intercept)"))
  foo <- dplyr::arrange(foo,
                        .data$outcome,
                        .data$term,
                        .data$level)

  foo

  # foo <- Reduce(cbind, x$cond.estimates)
  # bar <- Reduce(cbind, x$estimates)
  #
  # # foo[, colnames(bar)] <- bar # egad this is inelegant
  # foo <- cbind(bar, foo[, setdiff(colnames(foo), colnames(bar))]) # this is worse
  #
  # foo <- reshape2::dcast(reshape2::melt(foo), Var2 ~ Var1)
  #
  # colnames(foo) <- c("term", "estimate", "std.error")
  #
  # foo$statistic <- foo$estimate / foo$std.error
  # foo$p.value   <- 2 * stats::pnorm(abs(foo$statistic), lower.tail=FALSE) # package cjoint prescribes pnorm, not pt
  #
  # foo$outcome <- ifelse(sapply(stringr::str_detect(foo$term, as.character(x$respondent.varying)[1]), isTRUE),
  #                       "Difference", "Average Effect")
  # foo$term <- qdap::multigsub(paste0("^(.*:)?", foo$term[foo$outcome=="Average Effect"], "(:.*)?$"),
  #                             foo$term[foo$outcome=="Average Effect"], foo$term, fixed=FALSE)
  #
  # foo$level <- unlist(x$user.levels)[as.character(foo$term)] # ffs
  # # foo$term  <- qdap::multigsub(qdap::multigsub(names(x$user.names),  "", foo$term), "", foo$term)
  #
  # quux <- x$attributes
  # quux <- lapply(quux, list)
  # quux <- dplyr::as_tibble(quux)
  # quux <- tidyr::gather(quux, "newterm", "oldlevel")
  # quux <- tidyr::unnest(quux, .data$oldlevel)
  # quux <- dplyr::mutate(quux, term = paste0(.data$newterm, .data$oldlevel))
  #
  # foo <- dplyr::left_join(foo, quux)
  # foo <- dplyr::mutate(foo, term = ifelse(is.na(.data$newterm), .data$term, .data$newterm))
  # foo <- dplyr::select(foo, -.data$newterm, -.data$oldlevel)
  #
  # foo$level[sapply(stringr::str_detect(foo$term, as.character(x$respondent.varying)[1]), isTRUE)] <- "(Intercept)"
  #
  # ## the conditional differences
  #
  # ## the main AMCEs
  # # bar <- summary(x)
  # # colnames(bar$amce) <- c("term", "level", "estimate", "std.error", "statistic", "p.value", " ")
  #
  # # baz <- plyr::ldply(list(Main="estimates", Diff="cond.estimates"), .amce_tidify, x=x, .id="component")
  #
  # baz <- x$baselines
  # baz <- dplyr::as_tibble(baz)
  # baz <- tidyr::gather(baz, "term", "baseline")
  #
  # corge <- x$user.levels
  # corge <- dplyr::as_tibble(corge)
  # corge <- tidyr::gather(corge, "oldterm", "newlevel")
  # corge <- dplyr::left_join(corge, dplyr::select(quux, oldterm = .data$term, term = .data$newterm, .data$oldlevel))
  # corge <- dplyr::select(corge, -.data$oldterm)
  # corge <- dplyr::rename(corge, baseline = .data$oldlevel)
  #
  # baz <- dplyr::left_join(baz, corge)
  # baz <- dplyr::mutate(baz, baseline = ifelse(is.na(.data$newlevel), .data$baseline, .data$newlevel))
  # baz <- dplyr::select(baz, -.data$newlevel)
  #
  # foo <- dplyr::left_join(foo, baz)
  # foo <- dplyr::mutate(foo, baseline = ifelse(is.na(.data$baseline), "", .data$baseline))
  #
  # foo[, c("outcome", "term", "level", "baseline", setdiff(colnames(foo), c("outcome", "term", "level", "baseline")))]
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

#' @describeIn ascribe only number of respondents is clearly available
#' @export
ascribe.amce <- function(x, ...) {list(n=x$numrespondents)}

#' @describeIn unlevel returns NULL; this step is handled in the \code{\link{tidify}} method
#' @export
unlevel.amce <- function(x, ...) {NULL}

#' @describeIn delevel returns y; this step is handled in the \code{\link{tidify}} method
#' @export
delevel.amce <- function(x, y, z, ...) {y}
