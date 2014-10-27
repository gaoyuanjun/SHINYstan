#' Launch SHINYstan app (experimental)
#' 
#' This function will create an S4 object of class \code{SHINYstanfit} in the 
#' Global Environment and launch the SHINYstan app. 
#'  
#' @param object An object of class \code{stanfit} or \code{SHINYstanfit}.
#' @return In addition to launching the app this function returns S4 object of 
#' class \code{SHINYstanfit}. 
#' @seealso \code{\link[rstan]{stanfit-class}}, \code{\link[SHINYstan]{SHINYstanfit-class}}
#' @export
#' 
launch_SHINYstan2 <- function(object) {
  require(SHINYstan)
  
  is_stan <- function(X) inherits(X, "stanfit")
  is_SHINYstan <- function(X) inherits(X, "SHINYstanfit")
  launch <- function(object) {
    if (is_SHINYstan(object)) {
      shiny_stan_object <<- object  
    } 
    if (!is_SHINYstan(object)) {
      do.call("stanfit_to_SHINYstanfit", list(stanfit = object, make = TRUE)) 
    }
    shiny::runApp(system.file("SHINYstan", package = "SHINYstan"))  
  }
  rename_shiny_stan_object <- function(out_name, shiny_stan_object) {
    assign(out_name, shiny_stan_object, inherits = TRUE)
    shiny_stan_object <<- NULL
  }
  
  
  name <- deparse(substitute(object))
  if (missing(object)) {
    stop("Please specify a stanfit or SHINYstanfit object.")  
  }
  if (!is_stan(object) & !is_SHINYstan(object)) {
    stop(paste(name, "is not a stanfit or SHINYstanfit object."))
  }
  out_name <- ifelse(is_stan(object), paste0(name,"_shiny_stan"), name)
  on.exit(rename_shiny_stan_object(out_name, shiny_stan_object))
  launch(object)
}


