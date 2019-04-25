#' Tidily summarize a model, with metadata.
#'
#' The default method mostly wraps functions from \code{\link[broom]{broom}}.
#' @param x A model object.
#' @param margins logical. Serve up marginal effects from package \code{\link[margins]{margins-package}}? Default \code{FALSE} (output regression coefficients).
#' @param ... Ignored in the default method.
#' @return A \code{data.frame} with an added attribute or three.
#' @export
sumer <- function(x, margins=FALSE, ...) {UseMethod("sumer")}

#' Tabulate model results tidily.
#'
#' The default method returns \code{\link[broom]{tidy}}, which will work for all the most common models.
#'
#' @param x A model object.
#' @param margins logical. Serve up marginal effects from package \code{\link[margins]{margins-package}}? Default \code{FALSE} (output regression coefficients).
#' @param ... Ignored in the default method. Not passed to \code{tidy} in the default method.
#' @return A \code{data.frame}.
#' @export
tidify <- function(x, margins=FALSE, ...) {UseMethod("tidify")}

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


#' Finish the work started by \code{\link{unlevel}}.
#'
#' The default method will join \code{z} to \code{y} and return the relevant columns.
#' @param x A model object. Typically ignored (used only for method dispatch).
#' @param y Output of \code{\link{tidify}}.
#' @param z Output of \code{\link{unlevel}}.
#' @param ... Ignored in the default implementation.
#' @return a tidy presentation of results with separate term and level columns.
#' @export
delevel <- function(x, y, z, ...) {UseMethod("delevel")}

#' @importFrom rlang .data
#' @export
rlang::.data

