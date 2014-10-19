#' An S4 class for \code{SHINYstanfit} objects
#' 
SHINYstanfit <- setClass("SHINYstanfit", 
                         slots = list(model_name = "character",
                                      param_names = "character",
                                      param_groups = "character",
                                      samps_all = "array", 
                                      samps_post_warmup = "array",
                                      summary = "matrix",
                                      sampler_params = "list",
                                      nChains = "numeric",
                                      nIter = "numeric",
                                      nWarmup = "numeric"
                         ))


