#### helper functions ----
unpack <- function(foo, outcome) {
  if(length(foo) < 1) {
    return(dplyr::tibble(term = character(0), Level = character(0)))
  }

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
  foo <- dplyr::group_by(foo)

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

  lev <- names(x$user.levels) # work around case where there are colons in the dang factor levels
  lev <- lev[stringr::str_detect(lev, paste0("^", x$respondent.varying))]
  lev <- paste0(lev, collapse = "|")

  bar <- dplyr::mutate(bar, Group = stringr::str_remove(.data$Level, paste0("^(", lev, "):")))
  bar <- dplyr::mutate(bar, dplyr::across(c("term", "Level"), stringr::str_remove, pattern = ":.*$"))
  bar <- depack(bar, x$user.levels, "Group", "group")

  foo <- dplyr::bind_rows(foo, bar)
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
}

#' @describeIn ascribe only number of respondents is clearly available
#' @export
ascribe.amce <- function(x, ...) {list(n=x$numrespondents)}

#' @describeIn unlevel returns NULL; this step is handled in the \code{\link{tidify}} method
#' @export
unlevel.amce <- function(x, ...) {NULL}

#' @describeIn delevel returns y; this step is handled in the \code{\link{tidify}} method
#' @export
delevel.amce <- function(x, y, z, ...) {y}
