#' Launch SHINYstan app 
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the 
#' Global Environment and launch the SHINYstan app. 
#'  
#' @param object An object of class \code{SHINYstanfit} or \code{stanfit}.
#' @param ... Further arguments to be passed to \code{\link[SHINYstan]{stan2shinystan}} 
#' if \code{object} is a \code{stanfit} object.
#' @return If \code{object} is a \code{stanfit} object, in addition to launching
#' the app an object of class \code{SHINYstanfit} is returned.   
#' @seealso \code{\link[rstan]{stan}}
#' @export
#' 

launch_shinystan <- function(object, ...) {
  
  is_stan <- function(X) inherits(X, "stanfit")
  is_SHINYstan <- function(X) inherits(X, "SHINYstanfit")
  
  launch <- function(object) {
    if (is_SHINYstan(object)) {
      shiny_stan_object <<- object  
    } 
    if (!is_SHINYstan(object)) {
      do.call("stan2shinystan", list(stanfit = object, make = TRUE, ...)) 
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
  if (!is_stan(object) & !is_SHINYstan(object)) {
    stop(paste(name, "is not a stanfit or SHINYstanfit object."))
  }
  out_name <- ifelse(is_stan(object), paste0(name,"_shiny_stan"), name)
  on.exit(cleanup_shiny_stan(shiny_stan_object, out_name))
  launch(object)
}
