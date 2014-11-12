#' \code{shinystan} Objects
#'
#' @param X A \code{stanfit}, \code{mcmc.list}, or 3D array of
#' posterior samples. If \code{X} is not a \code{stanfit} or \code{mcmclist} object
#' then the dimensions of the 3D array must correspond to iterations, chains,
#' and parameters, in that order.
#' @param Y An object to test.
#' @param ... Additional arguments. See \strong{Details}, below, for instructions.
#' @return For \code{as.shinystan} an object of class \code{shinystan}. For
#' \code{is.shinystan} a logical value indicating whether the tested object
#' is a \code{shinystan} object.
#' @details If \code{X} is a \code{stanfit} object then no additional arguments
#' should be specified in \code{...}. If \code{X} is an \code{mcmc.list} or
#' 3D array of samples then the following arguments can be specified:
#' \describe{
#'   \item{\code{model_name}}{A character string giving a name for the model.}
#'   \item{\code{burnin}}{The number of warmup/burnin iterations.
#'   Not needed if the samples don't include any of the burnin.}
#'   \item{\code{param_dims}}{The dimensions for all parameters. A named list.}
#' }
#' @export
#'

as.shinystan <- function(X, ...) {
  get_type <- function(x) {
    if (inherits(x, "shinystan")) return("shinystan")
    if (inherits(x, "stanfit")) return("stanfit")
    if (inherits(x, "mcmclist")) return("mcmclist")
    return("other")
  }

  Xname <- deparse(substitute(X))
  what_X_is <- get_type(X)

  if (what_X_is == "shinystan") {
    print(paste0(Xname,
               " is already a shinystan object.", "\n",
               " You can use launch_shinystan(",Xname,") to launch the app."))
    return(X)
  }
  if (what_X_is == "stanfit") return(stan2shinystan(X, ...))
  if (what_X_is == "mcmclist") return(mcmc2shinystan(X, ...))
  if (what_X_is == "other") {
    if (!is.array(X)) stop(paste(Xname, "must be a stanfit, mcmc.list, or array."))
    array2shinystan(X, ...)
  }
}

#' @rdname as.shinystan
is.shinystan <- function(Y) inherits(Y, "shinystan")
