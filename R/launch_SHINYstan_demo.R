#' Launch SHINYstan app in demo mode
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the Global Environment
#' and then launch the SHINYstan app using the pre-loaded Stan example model "schools".
#' 
#' @seealso \code{\link[rstan]{stan_demo}} 
#' @export
#' 
launch_SHINYstan_demo <- function() {
  do.call("stanfit_to_SHINYstanfit", list(stanfit = schools, make = TRUE))    
  shiny::runApp(system.file("SHINYstan2", package = "SHINYstanDraft"))  
}




