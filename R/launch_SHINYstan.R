#' Launch SHINYstan app
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the Global Environment
#' and then launch the SHINYstan app. 
#' @param object An object of class \code{stanfit} or \code{SHINYstanfit}.
#' 
#' @seealso \code{\link[rstan]{stanfit-class}}, \code{\link[SHINYstan]{SHINYstanfit-class}}
#' @export
#' 
launch_SHINYstan <- function(object) {
  is_stan <- inherits(object, "stanfit")
  is_SHINYstan <- inherits(object, "SHINYstanfit")
  
  if (missing(object)) {
    stop("Please specify a stanfit or SHINYstanfit object.")  
  }
  if (!is_stan & !is_SHINYstan) {
    name <- deparse(substitute(object))
    stop(paste(name, "is not a stanfit or SHINYstanfit object."))
  }
  if (is_SHINYstan) {
    shiny_stan_object <<- object  
  } 
  if (!is_SHINYstan) {
    do.call("stanfit_to_SHINYstanfit", list(stanfit = object, make = TRUE)) 
  }
  shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
}
