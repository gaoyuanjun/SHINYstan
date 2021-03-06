#' Launch SHINYstan app in demo mode
#'
#' This function will create an S4 object of class \code{shinystan} in the Global Environment
#' and launch the SHINYstan app using a pre-loaded example model.
#'
#' @param demo_name The name of the demo model as a character string.
#' See \strong{Note} below for the available demos. If \code{demo_name} is
#' omitted it will default to the \code{eight_schools} Stan example model.
#'
#' @note List of available demo models:
#' \describe{
#' \strong{Stan}
#'   \item{\code{air}}{Berkson measurement error}
#'   \item{\code{beetles}}{Logit model}
#'   \item{\code{eight_schools}}{Multi-level linear model}
#'   \item{\code{pumps}}{Conjugate gamma-Poisson hierarchical model}
#' \strong{MCMCpack}
#' \item{\code{MCMMhpoisson}}{Hierarchical Poisson regression with log link}
#' }
#'
#' @return In addition to launching the app, an object of class \code{shinystan}
#' is returned.
#' @export
#' @examples
#' \dontrun{
#' launch_shinystan_demo() # launches "eight_schools" demo by default
#' launch_shinystan_demo("air")
#' }
#'
#'
launch_shinystan_demo <- function(demo_name = "eight_schools") {

  launch_demo <- function(object) {
    shiny_stan_object <<- object
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))
  }
  cleanup_shiny_stan <- function(shiny_stan_object, out_name) {
    assign(out_name, shiny_stan_object, inherits = TRUE)
    shiny_stan_object <<- NULL
    rm(list = "shiny_stan_object", envir = globalenv())
  }

  out_name <- paste0("shinystan_demo_", demo_name)
  on.exit(cleanup_shiny_stan(shiny_stan_object, out_name))

  if (demo_name == "air") launch_demo(air_demo_shiny_stan)
  if (demo_name == "beetles") launch_demo(beetles_demo_shiny_stan)
  if (demo_name == "eight_schools") launch_demo(eight_schools_demo_shiny_stan)
  if (demo_name == "pumps") launch_demo(pumps_demo_shiny_stan)
  if (demo_name == "MCMChpoisson") launch_demo(MCMChpoisson_demo_shiny_stan)
}

