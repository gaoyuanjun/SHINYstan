stan2shinystan <- function(stanfit, notes) {
  stopifnot(requireNamespace("rstan", quietly = TRUE))

  if (!inherits(stanfit, "stanfit")) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }

  samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)

  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- stanfit@model_name
  slots$param_names <- stanfit@sim$fnames_oi
  slots$param_dims <- stanfit@par_dims
  slots$param_groups <- names(stanfit@par_dims)
  slots$samps_all <- samps_all
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- nrow(samps_all) # total number of iterations (after thinning)
  slots$nWarmup <- floor(stanfit@sim$warmup / stanfit@sim$thin)
  if (!missing(notes)) slots$user_model_info <- notes

  do.call("new", slots)
}

