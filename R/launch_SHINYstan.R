#' Launch SHINYstan app
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the Global Environment
#' and then launch the SHINYstan app. 
#' @param stanfit An S4 object of class \code{\link[rstan]{stanfit-class}} (\pkg{rstan}).
#' 
#' @seealso \code{\link[rstan]{stan}} 
#' @export
#' 
launch_SHINYstan <- function(stanfit) {
  if (missing(stanfit)) {
    stop("Please specify a stanfit object.")  
  }
  if (!inherits(stanfit, "stanfit")) {
      name <- deparse(substitute(stanfit))
      stop(paste(name, "is not a stanfit object."))
  }
  do.call("stanfit_to_SHINYstanfit", list(stanfit = stanfit, make = TRUE))    
  shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
}



