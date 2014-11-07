split_rhat_rfun <- function (sims)  {
  if (is.vector(sims)) 
    dim(sims) <- c(length(sims), 1)
  chains <- ncol(sims)
  n_samples <- nrow(sims)
  half_n <- floor(n_samples/2)
  idx_2nd <- n_samples - half_n + 1
  split_chain_mean <- numeric(chains * 2)
  split_chain_var <- numeric(chains * 2)
  for (i in 1:chains) {
    split_chain_mean[i] <- mean(sims[1:half_n, i])
    split_chain_var[i] <- var(sims[1:half_n, i])
    split_chain_mean[chains + i] <- mean(sims[idx_2nd:n_samples, 
                                              i])
    split_chain_var[chains + i] <- var(sims[idx_2nd:n_samples, 
                                            i])
  }
  var_between <- half_n * var(split_chain_mean)
  var_within <- mean(split_chain_var)
  sqrt((var_between/var_within + half_n - 1)/half_n)
}