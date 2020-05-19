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
#' The default method returns \code{broom::\link[broom]{tidy}(lmtest::\link[lmtest]{coeftest}(x, ...))}.
#'
#' @param x A model object.
#' @param margins logical. Serve up marginal effects from package \code{\link[margins]{margins-package}}? Default \code{FALSE} (output regression coefficients).
#' @param ... other arguments to \code{margins::\link[margins]{margins}} or \code{lmtest::coeftest}.
#' @return A \code{data.frame}.
#' @export
tidify <- function(x, margins=FALSE, ...) {UseMethod("tidify")}

#' Separate model component from name of term in MLEs where scale and shape are modeled.
#'
#' The default method is intended for models of the mean only and simply returns the input.
#'
#' @param x A model object. Dispatch is on this argument.
#' @param y A data.frame put out by \code{\link{tidify}}.
#' @param ... Ignored in the default method. Probably ignored in most other methods, too.
#' @return A \code{data.frame}.
#' @export
encomp <- function(x, y, ...) {UseMethod("encomp")}

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

