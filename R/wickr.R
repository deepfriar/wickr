#' Tidily summarize a model, with metadata.
#'
#' The default method mostly wraps functions from \code{\link[broom]{broom}}.
#' @param x A model object.
#' @param ... Ignored in the default method.
#' @return A \code{data.frame} with an added attribute or three.
#' @export
sumer <- function(x, ...) {UseMethod("sumer")}

#' Tabulate model results tidily.
#'
#' The default method returns \code{\link[broom]{tidy}}, which will work for all the most common models.
#'
#' @param x A model object.
#' @param ... Ignored in the default method. Not passed to \code{tidy} in the default method.
#' @return A \code{data.frame}.
#' @export
tidify <- function(x, ...) {UseMethod("tidify")}

#' Model statistics.
#'
#' The default method returns \code{as.list(\link[broom]{glance}(x))}.
#' @param x A model object.
#' @param ... Ignored in the default implementation which returns \code{NA}.
#' @return a list.
#' @export
ascribe <- function(x, ...) {UseMethod("ascribe")}

#' Name of model.
#'
#' The default method returns \code{class(x)}.
#' @param x A model object.
#' @param ... Ignored in the default implementation which returns \code{NA}.
#' @return a character vector.
#' @export
kind <- function(x, ...) {UseMethod("kind")}

#' Replace concatenated term and level with separate columns.
#'
#' The default method will unpack \code{x$xlevels}.
#' @param x A model object.
#' @param ... Ignored in the default implementation.
#' @return a data.frame.
#' @export
unlevel <- function(x, ...) {UseMethod("unlevel")}
