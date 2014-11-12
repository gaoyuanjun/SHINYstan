#' Launch SHINYstan app
#'
#' @param object An object of class \code{shinystan} or \code{stanfit}.
#' @return If \code{object} is a \code{stanfit} object then, in addition to launching
#' the SHINYstan app, this function creates an object of class \code{shinystan} in
#' the Global Environment.
#' @export
#' @examples
#' \dontrun{
#' # If X is a shinystan object or stanfit object then to launch the app
#' # just run
#' launch_shinstan(X)
#'
#' # If X is an mcmc.list object or 3D array then to launch the app first
#' # convert X to a shinystan object using the as.shinystan function
#' X_shinystan <- as.shinystan(X)
#' launch_shinystan(X_shinystan)
#'
#' }
#'

launch_shinystan <- function(object) {

  is_stan <- function(X) inherits(X, "stanfit")

  launch <- function(object) {
    if (is.shinystan(object)) {
      shiny_stan_object <<- object
    }
    if (!is.shinystan(object)) {
      shiny_stan_object <<- stan2shinystan(object)
    }
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))
  }

  cleanup_shiny_stan <- function(shiny_stan_object, out_name) {
    assign(out_name, shiny_stan_object, inherits = TRUE)
    shiny_stan_object <<- NULL
    rm(list = "shiny_stan_object", envir = globalenv())
  }

  name <- deparse(substitute(object))
  if (missing(object)) {
    stop("Please specify a stanfit or SHINYstanfit object.")
  }
  if (!is_stan(object) & !is.shinystan(object)) {
    stop(paste(name, "is not a stanfit or shinystan object."))
  }
  out_name <- ifelse(is_stan(object), paste0(name,"_shiny_stan"), name)
  on.exit(cleanup_shiny_stan(shiny_stan_object, out_name))
  launch(object)
}
