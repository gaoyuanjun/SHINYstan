#' An S4 class for \code{shinystan} objects
#'
shinystan <- setClass("shinystan",
                      # Slots
                         slots = list(model_name = "character",
                                      param_names = "character",
                                      param_groups = "character",
                                      param_dims = "list",
                                      samps_all = "array",
                                      samps_post_warmup = "array",
                                      summary = "matrix",
                                      sampler_params = "list",
                                      nChains = "numeric",
                                      nIter = "numeric",
                                      nWarmup = "numeric",
                                      user_model_info = "character"
                                      ),
                      # Prototype
                         prototype = list(model_name = "No name",
                                      param_names = "",
                                      param_groups = "",
                                      param_dims = list(),
                                      samps_all = array(NA, c(1,1)),
                                      samps_post_warmup = array(NA, c(1,1)),
                                      summary = matrix(NA, nr=1,nc=1),
                                      sampler_params = list(),
                                      nChains = 0,
                                      nIter = 0,
                                      nWarmup = 0,
                                      user_model_info = ""
                                      )
                           )



