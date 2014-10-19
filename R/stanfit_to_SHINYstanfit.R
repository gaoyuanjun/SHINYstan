#' Convert an object of class \code{stanfit} to class \code{SHINYstanfit}
#' 
#' @export
#' 
stanfit_to_SHINYstanfit <- function(stanfit, make = FALSE) {
  slots <- list()
  slots$Class <- "SHINYstanfit"
  slots$model_name <- stanfit@model_name
  slots$param_names <- stanfit@sim$fnames_oi
  slots$param_groups <- stanfit@model_pars[-length(stanfit@model_pars)]
  slots$samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  slots$samps_post_warmup <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = FALSE)
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- stanfit@sim$iter
  slots$nWarmup <- stanfit@sim$warmup
  
  if (!make) {
    out <- do.call("new", slots)
    return(out)
  }
  
  shiny_stan_object <<- do.call("new", slots)
}