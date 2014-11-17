# Extract the content of the shiny_stan_object slots
object <- shiny_stan_object
samps_all <- object@samps_all
sampler_params <- object@sampler_params
nIter <- object@nIter
nChains <- object@nChains
warmup_val <- object@nWarmup
samps_post_warmup <- samps_all[(warmup_val + 1):nIter,,]
# samps_post_warmup <- object@samps_post_warmup
fit_summary <- object@summary
param_names <- object@param_names
