#' Convert an object of class \code{stanfit} (\pkg{rstan}) to class \code{SHINYstanfit}
#' 
#' @param stanfit A \code{stanfit} object. 
#' @param notes A character string. 
#' @param make .
#' @seealso \code{\link[rstan]{stan}}
#' @export
#' 

stan2shinystan <- function(stanfit, notes, make = FALSE) {
  stopifnot(requireNamespace("rstan", quietly = TRUE))
  
  if (!inherits(stanfit, "stanfit")) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }
  
  slots <- list()
  slots$Class <- "SHINYstanfit"
  slots$model_name <- stanfit@model_name
  slots$param_names <- stanfit@sim$fnames_oi
  slots$param_dims <- stanfit@par_dims
  slots$param_groups <- names(stanfit@par_dims)
  slots$samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  slots$samps_post_warmup <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = FALSE)
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- stanfit@sim$iter
  slots$nWarmup <- stanfit@sim$warmup
  if (!missing(notes)) slots$user_model_info <- notes  

  if (!make) {
    out <- do.call("new", slots)
    return(out)
  }
  
  shiny_stan_object <<- do.call("new", slots)
}