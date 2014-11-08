#' Convert an array of posterior samples to class \code{shinystan}
#'
#' @param X A 3D array of posterior samples, where the dimensions are
#' iterations, chains, and parameters, in that order.
#' @param model_name A character string giving a name for the model.
#' @param burnin The number of warmup/burnin iterations. Not needed if the samples don't include any of the burnin.
#' @param param_dims The dimensions for all parameters. A named list.
#' @export
#'
#'
array2shinystan <- function(X, model_name = "unnamed model", burnin = 0, param_dims = list()) {

  Xname <- deparse(substitute(X))
  if (!is.array(X)) {
    stop (paste(Xname, "is not an array"))
  }
  if (length(dim(X)) != 3) {
    stop (paste(Xname, "must be an array with 3 dimensions"))
  }

  if (is.null(dimnames(X)[[3]])) {
    dimnames(X)[[3]] <- paste0("V", 1:dim(X)[3])
  }

  dimnames(X) <- list(iterations = 1:nrow(X),
                                chains = paste0("chain:",1:ncol(X)),
                                parameters = dimnames(X)[[3]])
  X_post_warmup <- X[(burnin+1):nrow(X),,]
  param_names <- dimnames(X)[[3]]
  param_dims <- param_dims
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
  slots$Class <- "shinystan"
  slots$model_name <- model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- param_groups
  slots$samps_all <- X
  slots$samps_post_warmup <- X_post_warmup
  slots$summary <- shinystan_monitor(X, warmup = burnin)
  slots$sampler_params <- list(NA)
  slots$nChains <- ncol(X)
  slots$nIter <- nrow(X)
  slots$nWarmup <- burnin

  do.call("new", slots)
}
