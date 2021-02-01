#' @describeIn sumer tibbles with list columns of models
#' @param .depth integer. Levels from top of nested list of objects. Default \code{0}.
#' @export
sumer.tbl <- function(x, margins=FALSE, .depth=0,  ...) {
  x <- x[, order(sapply(x, is.list))]

  v <- colnames(x)[which( sapply(x, is.list))]

  if(length(v) < 1) {stop("Can't handle a tibble with no list columns.")}

  x <- tidyr::gather(x, "Column", "entry", .data[[v[[1]]]]:.data[[v[[length(v)]]]])
  x <- dplyr::filter(x, !sapply(.data$entry, is.null)) # TEST: can this go before the grouping part instead of after?

  # NEW 2020-05-19: add a column numbering the entries\
  x <- dplyr::group_by_if(x, function(i) {!is.list(i)})
  x <- dplyr::mutate(x, Row = 1:dplyr::n())
  x <- dplyr::group_by(x, .data$Row, add = TRUE)
  x <- dplyr::select(x, .data$entry)

  x <- dplyr::group_by(x)
  x <- `colnames<-`(x, stringr::str_replace_all(colnames(x), "^Column$", paste("Column", .depth)))
  x <- `colnames<-`(x, stringr::str_replace_all(colnames(x), "^Row$",    paste("Row",    .depth)))
  x <- dplyr::group_by_if(x, function(i) {!is.list(i)})

  # dplyr::first(.data$exntry) is the key -- this is where the recursion really happens
  y <- dplyr::mutate(x, entry = list(sumer(dplyr::first(.data$entry), margins=margins, .depth = .depth + 1, ...)))
  z <- dplyr::mutate(y, entry = list(purrr::flatten_dfc(attr(dplyr::first(.data$entry), "sumer"))))

  y <- tidyr::unnest(y, .data$entry)
  z <- tidyr::unnest(z, .data$entry)

  attr(y, "sumer") <- z
  class(y) <- c("sumer", class(y))

  y
}
