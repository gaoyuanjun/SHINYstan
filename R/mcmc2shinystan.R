#' Convert an object of class \code{mcmc.list} to class \code{SHINYstanfit}
#' 
#' @param X An \code{mcmc.list}
#' @param info An optional named list of additional information. See \strong{Details}
#' below.    
#' @details \code{info} is an optional named list containing 
#' \describe{
#'   \item{\code{model_name}}{A name for the model}
#'   \item{\code{burnin}}{The number of burnin iterations. Not needed if the 
#'   samples don't include any of the burnin.}
#'   \item{\code{param_dims}}{The dimensions for all parameters. A named list.}
#' }  
#' @export
#' @seealso \code{\link[coda]{mcmc.list}}
#' 
mcmc2shinystan <- function(X, info = list(model_name = deparse(substitute(X)),
                                          burnin = 0,
                                          param_dims = list())) {

  stopifnot(requireNamespace("coda", quietly = TRUE))
  
  Xname <- deparse(substitute(X))
  if (!inherits(X, "mcmc.list")) {
    stop (paste(Xname, "is not an mcmc.list."))
  }
  
  samps_array <- aperm(as.array(X), c(1,3,2))
  dimnames(samps_array) <- list(iterations = 1:nrow(samps_array), 
                                chains = paste0("chain:",1:ncol(samps_array)), 
                                parameters = dimnames(samps_array)[[3]])
  samps_array_post_warmup <- samps_array[(info$burnin+1):nrow(samps_array),,]
  param_names <- dimnames(X[[1]])[[2]]
  param_dims <- info$param_dims
  if (length(param_dims) == 0) {
    param_dims <- list()
    param_dims[1:length(param_names)] <- NA
    names(param_dims) <- param_groups <- param_names
    for(i in 1:length(param_names)) {
      param_dims[[i]] <- numeric(0) 
    }
  } else {
    param_groups <- names(param_dims)
  }
  slots <- list()
  slots$Class <- "SHINYstanfit"
  slots$model_name <- info$model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- param_groups
  slots$samps_all <- samps_array
  slots$samps_post_warmup <- samps_array_post_warmup
  slots$summary <- shinystan_monitor(samps_array, warmup = info$burnin)
  slots$sampler_params <- list(NA)
  slots$nChains <- ncol(samps_array)
  slots$nIter <- nrow(samps_array)
  slots$nWarmup <- info$burnin
  
  
  do.call("new", slots)
}

