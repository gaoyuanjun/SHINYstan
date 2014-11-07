# Extract the content of the shiny_stan_object slots
object <- shiny_stan_object
samps_all <- object@samps_all
samps_post_warmup <- object@samps_post_warmup
sampler_params <- object@sampler_params
nIter <- object@nIter
warmup_val <- object@nWarmup
fit_summary <- object@summary
param_names <- object@param_names
