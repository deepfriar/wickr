#' @describeIn sumer tibbles with list columns of models
#' @export
sumer.tbl <- function(x, margins=FALSE, ...) {
  x <- x[, order(sapply(x, is.list))]

  v <- colnames(x)[which( sapply(x, is.list))]

  if(length(v) < 1) {stop("Can't handle a tibble with no list columns.")}

  x <- tidyr::gather(x, "Column", "entry", .data[[v[[1]]]]:.data[[v[[length(v)]]]])
  x <- dplyr::group_by_if(x, function(i) {!is.list(i)})

  x <- dplyr::filter(x, !sapply(.data$entry, is.null))

  y <- dplyr::mutate(x, entry = list(wickr::sumer(dplyr::first(.data$entry), margins=margins, ...)))
  z <- dplyr::mutate(y, entry = list(purrr::flatten_dfc(attr(dplyr::first(.data$entry), "sumer"))))

  y <- tidyr::unnest(y)
  z <- tidyr::unnest(z)

  attr(y, "sumer") <- z
  class(y) <- c("sumer", class(y))

  y
}
