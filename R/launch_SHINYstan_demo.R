#' Launch SHINYstan app in demo mode
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the Global Environment
#' and then launch the SHINYstan app using a pre-loaded example model.
#' 
#' @param demo_name Character string giving the name of the Stan demo model. 
#' See \strong{Note} below for the available demos. If \code{demo_name} is 
#' omitted it will default to the \code{eight_schools} Stan example model. 
#' 
#' @note List of available demo models: 
#' \describe{
#'   \item{\code{air}}{Berkson measurement error}
#'   \item{\code{beetles}}{Logit model}
#'   \item{\code{eight_schools}}{Multi-level linear model}
#'   \item{\code{pumps}}{Conjugate gamma-Poisson hierarchical model}
#' } 
#' 
#' @seealso \code{\link[rstan]{stan_demo}} 
#' @export
#' @examples
#' \dontrun{
#' launch_SHINYstan_demo("eight_schools")
#' }
#' 
launch_SHINYstan_demo <- function(demo_name) {
  if (missing(demo_name)) {
    do.call("stanfit_to_SHINYstanfit", list(stanfit = eight_schools, make = TRUE))
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
  } else {
    demo_name <- get(demo_name)
    do.call("stanfit_to_SHINYstanfit", list(stanfit = demo_name, make = TRUE))
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
  }
}

