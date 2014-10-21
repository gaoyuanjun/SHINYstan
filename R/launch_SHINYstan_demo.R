#' Launch SHINYstan app in demo mode
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the Global Environment
#' and then launch the SHINYstan app using a pre-loaded Stan example model.
#' 
#' @param demo_name The name of the demo model. If \code{demo_name} is omitted it will default
#' to the "eight schools" Stan example model. See 'Note' below for the available demos. 
#' 
#' @note List of available demo models: 
#' \describe{
#'   \item{eight_schools}{Eight Schools}
#'   \item{binormal}{Binormal}
#'   \item{normal}{Normal}
#' } 
#' 
#' @seealso \code{\link[rstan]{stan_demo}} 
#' @export
#' 
launch_SHINYstan_demo <- function(demo_name) {
  if (missing(demo_name)) {
    do.call("stanfit_to_SHINYstanfit", list(stanfit = eight_schools, make = TRUE))
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
  } else {
    if (is.character(demo_name)) demo_name <- get(demo_name)
    do.call("stanfit_to_SHINYstanfit", list(stanfit = demo_name, make = TRUE))
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
  }
}





