#' @describeIn sumer tibbles with list columns of models
#' @param .depth integer. Levels from top of nested list of objects. Default \code{0}.
#' @export
sumer.tbl <- function(x, .depth=0, margins=FALSE, ...) {
  x <- x[, order(sapply(x, is.list))]

  v <- colnames(x)[which( sapply(x, is.list))]

  if(length(v) < 1) {stop("Can't handle a tibble with no list columns.")}

  x <- tidyr::gather(x, "Column", "entry", .data[[v[[1]]]]:.data[[v[[length(v)]]]])
  x <- dplyr::filter(x, !sapply(.data$entry, is.null)) # TEST: can this go before the grouping part instead of after?

  # NEW 2020-05-19: add a column numbering the entries
  # x <- dplyr::group_by(x, .data$Column)
  # x <- dplyr::mutate(x, Level_ = 1:dplyr::n())
  # x <- dplyr::group_by(x, .data$Level_)
  # x <- dplyr::select(x, -.data$Level_)
  x <- dplyr::group_by_if(x, function(i) {!is.list(i)})

  # dplyr::first(.data$exntry) is the key -- this is where the recursion really happens
  y <- dplyr::mutate(x, entry = list(wickr::sumer(dplyr::first(.data$entry), margins=margins, ...)))
  z <- dplyr::mutate(y, entry = list(purrr::flatten_dfc(attr(dplyr::first(.data$entry), "sumer"))))

  y <- tidyr::unnest(y)
  z <- tidyr::unnest(z)

  attr(y, "sumer") <- z
  class(y) <- c("sumer", class(y))

  y
}
