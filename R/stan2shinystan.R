stan2shinystan <- function(stanfit, notes) {
  stopifnot(requireNamespace("rstan", quietly = TRUE))

  if (!inherits(stanfit, "stanfit")) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }

  samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  param_names <- dimnames(samps_all)[[3]]
  param_dims <- get_stanfit_param_dims(stanfit, param_names)

  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- stanfit@model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- names(param_dims)
<<<<<<< HEAD
=======
  slots$param_names <- stanfit@sim$fnames_oi
  slots$param_dims <- stanfit@par_dims
  slots$param_groups <- names(stanfit@par_dims)
>>>>>>> FETCH_HEAD
  slots$samps_all <- samps_all
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- nrow(samps_all) # total number of iterations (after thinning)
  slots$nWarmup <- floor(stanfit@sim$warmup / stanfit@sim$thin)
  if (!missing(notes)) slots$user_model_info <- notes

  do.call("new", slots)
}

