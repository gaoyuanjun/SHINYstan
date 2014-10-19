# reused functions --------------------------------------------------------
.in_range <- function(x, a, b) {
  x >= a & x <= b
}


# reused ggplot theme elements --------------------------------------------
fat_axis <- theme(axis.line = element_line(size = 1.5))
h_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.x = element_blank())
v_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.y = element_blank())
no_lgnd <- theme(legend.position = "none")



# find_stan --------------------------------------------------------------
.find_stan <- function() {
  is.stanfit <- function(X) inherits(X, "stanfit")
  objs <- mget(ls(envir = .GlobalEnv), envir = .GlobalEnv)
  stanfits <- names(Filter(is.stanfit, objs))  
  return(stanfits)
}

# check_stan --------------------------------------------------------------
.check_stan <- function() {
  found <- .find_stan()
  stanfit_in_found <- "stanfit" %in% found
  multiple_stanfits <- length(found) > 1
  no_stanfits <- length(found) == 0
  if (multiple_stanfits & !stanfit_in_found) return("multiple")
  if (no_stanfits) return("none")
  return("ok")
}

# get_stan ---------------------------------------------------------------
.get_stan <- function() {
  stopifnot(.check_stan() == "ok")
  found <- .find_stan()
  stanfit_in_found <- "stanfit" %in% found
  if (stanfit_in_found) {
    return(get("stanfit"))
  }
  return(get(found))
}


# get_par_groups ----------------------------------------------------------
.get_par_groups <- function(stanfit, include_lp = TRUE) {
  groups <- stanfit@model_pars
  if (include_lp) {
    return(groups)
  }
  return(groups[-which(groups == "lp__")])
}


# make_parlist ------------------------------------------------------------
.make_parlist <- function(stanfit, include_lp = TRUE) {
  names <- sort(stanfit@sim$fname)
  names <- names[-which(names == "lp__")]
  if (include_lp) {
    names <- c(names, "lp__")
  }
  parlist <- as.list(names)
  names(parlist) <- names
  return(parlist)
}


# check_stanfit ------------------------------------------------------------------
.check_stanfit <- function(stanfit) {
  if (stanfit@mode == 1L) {
    cat("Stan model '", stanfit@model_name, "' is of mode 'test_grad';\n", 
        "sampling is not conducted.\n", sep = "")
    return(invisible(NULL))
  }
  else if (stanfit@mode == 2L) {
    cat("Stan model '", stanfit@model_name, "' does not contain samples.\n", 
        sep = "")
    return(invisible(NULL))
  }
  if (isTRUE(all.equal(stanfit@sim$n_save, stanfit@sim$warmup2, 
                       check.attributes = FALSE, 
                       check.names = FALSE))) {
    cat("Stan model '", stanfit@model_name, "' does not contain samples after warmup.\n", 
        sep = "")
    return(invisible(NULL))
  }
}


# get_model_info ------------------------------------------------------------
.get_model_info <- function(stanfit) {
  n_chains <- ncol(stanfit)
  date <- stanfit@date
  date <- gsub("  "," ", date)
  n_warm <- stanfit@sim$warmup
  n_iter <- stanfit@sim$iter
  out <- list(Date = date, Chains = n_chains, Iterations = n_iter, Warmup = n_warm)
  return(out)
}


# param_trace -------------------------------------------------------------
.param_trace <- function(param, dat, warmup_val, chain, x1, x2, y1, y2) {
  dat <- melt(dat)
  
  if (chain != 0) {
    dat <- subset(dat, chains == paste0("chain:",chain))
  }
  
  warmup_rect <- annotate("rect", xmin = Inf, xmax = warmup_val, 
                          ymin = -Inf, ymax = Inf, fill = "skyblue", alpha = 0.15)
  
  my_labs <- labs(y = param, x = "Iteration\n Warmup | Samples")
  gg_trace <- ggplot(dat, aes(x = iterations, y = value, color = chains))
  gg_trace <- (gg_trace + my_labs + warmup_rect + 
#                  geom_vline(xintercept = warmup_val, size = 2) + 
                 geom_line() + 
                 theme_classic() %+replace% (fat_axis + h_lines + no_lgnd))
  if (is.na(x1)) {
    return(gg_trace)
  } else {
    gg_trace <- (gg_trace + 
                   scale_y_continuous(limits = c(y1, y2)) + 
                   scale_x_continuous(limits = c(x1, x2)))
    return(gg_trace)
  }
}


# param_dens --------------------------------------------------------------
# .param_dens <- function(param, dat, warmup_val, chain, fill_color = NULL, point_est) {
#   dat <- subset(melt(dat), iterations > warmup_val)
#   
#   if (chain != 0) {
#     dat <- subset(dat, chains == paste0("chain:",chain))
#   }
#   
#   Mean <- mean(dat$value)
#   Median <- median(dat$value)
#   densx <- density(dat$value)$x
#   densy <- density(dat$value)$y
#   MAP <- densx[which.max(densy)]
#   #     ynd <- max(density(dat$value)$y)/10
#   
#   clr <- ifelse(is.null(fill_color), "black", fill_color)
#   
#   gg_dens <- ggplot(dat, aes(x = value))
#   gg_dens <- (gg_dens + 
#                 labs(x = param, y = "") +
#                 geom_density(fill = clr, color = clr) + 
#                 ggtitle("Posterior Density (post-warmup) \n") + 
#                 theme_classic() %+replace% (fat_axis + h_lines))
#   
#   if (point_est != "None") {
#     gg_dens <- gg_dens + geom_segment(x = get(point_est), xend = get(point_est),
#                                       y = 0, yend = max(densy),
#                                       color = "lightgray", lwd = 1, lty = 2)
#   }
#   gg_dens
# }

.param_dens <- function(param, dat, warmup_val, chain, 
                        fill_color = NULL, line_color = NULL, 
                        point_est, CI) {
  
  dat <- melt(dat)
  
  if (chain != 0) {
    dat <- subset(dat, chains == paste0("chain:",chain))
  }
  
  Mean <- mean(dat$value)
  Median <- median(dat$value)
  dens_dat <- with(density(dat$value), data.frame(x,y))
  MAP <- with(dens_dat, x[which.max(y)])
  
  fclr <- ifelse(is.null(fill_color), "black", fill_color)
  lclr <- ifelse(is.null(line_color), "lightgray", line_color)
  
  gg_dens <- ggplot(dens_dat, aes(x = x, ymax = y))
  gg_dens <- (gg_dens + 
                labs(x = param, y = "") +
                geom_ribbon(ymin = 0, fill = fclr, color = fclr) + 
                ggtitle("Posterior Density (post-warmup) \n") + 
                theme_classic() %+replace% (fat_axis + h_lines))

  if (point_est != "None") {
    gg_dens <- gg_dens + annotate("segment", 
                                x = get(point_est), xend = get(point_est),
                                y = 0, yend = max(dens_dat$y),
                                color = lclr, lwd = 1, lty = 2)
  }
  if (CI != "None") {
    lev <- (1 - as.numeric(CI)/100)/2
    quant <- quantile(dat$value, probs = c(lev, 1 - lev))
    gg_dens <- (gg_dens + 
                annotate("segment", x = quant, xend = quant, y = 0, yend = max(dens_dat$y), color = lclr, lty = rep(1:length(CI),2))
    )
  }
  gg_dens
}


# param_contour -----------------------------------------------------------
.param_contour <- function(samps, param, param2, type, high_color, low_color, nBins) {
  params <- c(param, param2)
  nParams <- 2
  nIter <- dim(samps)[1] * dim(samps)[2]
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params
  dat <- data.frame(x = samps.use[,param], y = samps.use[,param2])
  g <- ggplot(dat, aes(x = x, y = y)) + labs(x = param, y = param2)      
  if (type == "Point") {
    g <- (g + stat_density2d(geom="point", aes(size = ..density.., color = ..density..), contour = FALSE) +
      scale_color_gradient(low = low_color, high = high_color))
  }
  if (type == "Contour") {
    g <- (g + stat_density2d(geom="path", aes(color = ..level..), bins = nBins, contour = TRUE) + 
       scale_color_gradient(low = low_color, high = high_color))
  }
  g + theme_classic() %+replace% (no_lgnd + fat_axis)
}



# param_summary -----------------------------------------------------------
.param_summary <- function(dat, param, r_e, warmup_val) {
  dat <- melt(dat)
  value <- dat$value
  ds <- function(d) {
    q <- quantile(d, probs = c(0.025,0.5, 0.975))
    m <- mean(d)
    sd <- sd(d)
    out <- c(r_e, Mean = m, SD = sd, q)
    outmat <- matrix(out, 1, length(out))
    colnames(outmat) <- names(out)
    rownames(outmat) <- NULL
    outmat
  }
  ds(value)
}


# all_summary -------------------------------------------------------------
.all_summary <- function(fit_summary) {
  out <- round(fit_summary, 2)
  out[,"n_eff"] <- round(out[,"n_eff"])
  nc <- ncol(out)
  out <- out[,c(nc, nc-1, 1:(nc-2))]
  cbind(Parameter = rownames(out), out)
}



# coef_plot ---------------------------------------------------------------
.coef_plot <- function(stanfit, params, point, ci_level) {
  a <- 1 - (1 - ci_level)/2
  dat <- summary(stanfit, pars = params, probs = c(0.5, a, 1-a))$summary
  dat <- dat[-nrow(dat), ]
  dat <- cbind(parameter = rownames(dat), as.data.frame(dat))
  colnames(dat)[which(colnames(dat)=="50%")] <- "median"
  colnames(dat)[which(colnames(dat)==paste0(a*100,"%"))] <- "upper"
  colnames(dat)[which(colnames(dat)==paste0((1-a)*100,"%"))] <- "lower"
  
  if (point == "mean") {
    dat <- mutate(dat, lb = mean - qnorm(a)*sd, ub = mean + qnorm(a)*sd)
    gg <- ggplot(dat, aes(x = parameter, y = mean, 
                          ymin = lb, ymax = ub, color = Rhat))
  }
  if (point == "median") {
    gg <- ggplot(dat, aes(x = parameter, y = median, 
                          ymin = lower, ymax = upper, color = Rhat))
  }
  no_labs <- labs(x = "", y = "")
  rhat_colors <- scale_color_continuous(low = "blue", high = "red")
  axis_text <- theme(axis.text.y = element_text(face = "bold", color = "black"))
  lgnd_pos <- theme(legend.position = "top")
  gg <- (gg + 
           no_labs + rhat_colors + axis_text + 
           geom_pointrange() + 
           coord_flip() )
  gg + theme_linedraw() %+replace% (lgnd_pos + fat_axis)
}


# plot_param_vertical ------------------------------------------------------------
.plot_param_vertical <- function(samps, params = NULL, show.options,
                                CI.level = 0.5, show.level = 0.95, point_est,
                                fill_color, outline_color, segment_color, est_color){
  
  show.density <- "density" %in% show.options
  show.lines <- "lines" %in% show.options
  
  dim.samps <- dim(samps) #nIter, nChain, nParam
  if(length(params) == 0) {
    params = dimnames(samps)$parameters[1:min(10, dim.samps[3])]
  }
  nParams <- length(params)
  nIter <- dim.samps[1] * dim.samps[2]
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params
  
  samps.mean <- apply(samps.use, 2, mean)
  samps.median <- apply(samps.use, 2, median)
  probs.use <- c(0.5 - show.level / 2, 
                 0.5 - CI.level / 2, 
                 0.5,
                 0.5 + CI.level / 2, 
                 0.5 + show.level / 2)
  samps.quantile <- t(apply(samps.use, 2, quantile, probs = probs.use))
  
  y <- seq(nParams, 1, by = -1)
  xlim <- c(min(samps.quantile[,1]), max(samps.quantile[,5])) 
  xrange <- diff(xlim)
  xlim[1] <- xlim[1] - 0.05 * xrange
  xlim[2] <- xlim[2] + 0.05 * xrange
  par(mar = c(1.5,5,3,1))
  
  plot(samps.median, y, bty = "n", type = "n", axes = FALSE,
       xlim = xlim, pch = 20, ylim = c(0.5, nParams + 1),
       xlab = "", ylab = "")
  
  abline(h = y, lty = 2, col = "lightgray")
  grid(nx = NULL, ny = 0, lty = 2)
  
#   axis(side = 2, at = y, labels = params, las = 1)
  mtext(text = params, side = 2, las = 1, at = y, font = 2)
  axis(side = 3, lwd = 3)
  
  if(show.density){
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i], 
                        from = samps.quantile[i,1],
                        to = samps.quantile[i,5])
      y.max <- max(d.temp$y)
      x.plot <- d.temp$x
      y.plot <- d.temp$y / y.max * 0.8 + y[i]
      
      
      d.poly <- density(samps.use[,i], 
                        from = samps.quantile[i,2], 
                        to = samps.quantile[i,4])
      x.poly <- d.poly$x
      y.poly.max <- max(d.poly$y)
      y.poly <- d.poly$y / y.poly.max * 0.8 + y[i]
      
      polygon(x = c(min(x.poly), x.poly, max(x.poly)), 
              y = c(y[i], y.poly, y[i]),
              density=100, col = fill_color)
      
      lines(x.plot, y.plot, col = outline_color)
#       d.line <- density(samps.use[,i], 
#                         from = samps.quantile[i,2], 
#                         to = samps.quantile[i,4], n = 2)
#       x.plot <- d.line$x
#       y.plot <- d.line$y / y.max * 0.8 + y[i]
#       segments(x.plot, y[i], x.plot, y.plot, lty = 3)

      if (point_est == "Median") {
        segments(samps.median[i], y[i], samps.median[i], y[i] + 0.25,  lwd = 2, col = est_color)  
      }
      if (point_est == "Mean") {
        segments(samps.mean[i], y[i], samps.mean[i], y[i] + 0.25,  lwd = 2, col = est_color)  
      }
      segments(samps.quantile[,1], y, samps.quantile[,5], y, col = outline_color) 
    }
  }
  else{
    if (show.lines) {
      segments(samps.quantile[,1], y, samps.quantile[,5], y, col = outline_color) 
    }
    segments(samps.quantile[,2], y, samps.quantile[,4], y, lwd = 3, col = fill_color)
    if (point_est == "Median") {
      points(samps.median, y, pch = 20, cex = 1.25, col = est_color)
    }
    if (point_est == "Mean") {
      points(samps.mean, y, pch = 20, cex = 1.25, col = est_color)
    }
  }
}



# rhat_plot ---------------------------------------------------------------
.calc_height_fixed <- function(pars) {
  N <- length(pars)
  N <- ifelse(N < 20, 20, N)
  round(400*N/20)
}

.rhat_plot <- function(fit_summary) {  
  dat <- fit_summary[,"Rhat"]
  dat <- data.frame(parameter = names(dat), Rhat = dat)
  dat <- mutate(dat, val = ifelse(.in_range(Rhat, 0.95, 1.05), "low",
                                  ifelse(Rhat <= 1.10, "high","very high")))
  dat$val <- factor(dat$val, levels = c("low","high","very high"))
  min_rhat <- min(dat$Rhat)
  max_rhat <- max(dat$Rhat)
  y_lim <- c(min_rhat, max(1.2, max_rhat))
  y_scale <- scale_y_continuous(limits = y_lim)
  my_labs <- labs(x = "", y = bquote(hat(R)))
  lgnd_txt <- theme(legend.text = element_text(size = 14), legend.key.size = unit(1, "cm"), legend.text.align = 0, legend.position = "top")
  my_clrs <- scale_color_manual(name = "", values = c("orange", "orangered", "darkorange"), 
                                labels = c(expression(hat(R) < 1.05), expression(hat(R) < 1.10),expression(hat(R) > 1.10)))
  axis_text <- theme(axis.text.y = element_text(face = "bold", color = "black"))
  gg_rhat <- ggplot(dat, aes(x = parameter, y = Rhat, ymax = Rhat, color = val))
  gg_rhat <- (gg_rhat + geom_pointrange(ymin = 1) + 
                y_scale + my_labs + my_clrs + 
                coord_flip()
              )
  gg_rhat + theme_classic() %+replace% (lgnd_txt + v_lines + axis_text + fat_axis)
}


# sampler_summary ---------------------------------------------------------
.sampler_summary <- function(sampler_params, inc_warmup, warmup_val) {
  sampler_stuff <- function(param) {  
    X <- sampler_params
    if (inc_warmup == TRUE) {
      X <- lapply(1:length(sampler_params), function(i) {
        out <- sampler_params[[i]] 
        out[-(1:warmup_val), ]
      })
    }
    means <- sapply(X, FUN = function(x) mean(x[,param]) )
    names(means) <- paste0("chain",1:length(means))
    means
  }
  params <- colnames(sampler_params[[1]])
  out <- sapply(params, FUN = function(i) sampler_stuff(param = i))
  out <- rbind("All chains" = colMeans(out), out)
  colnames(out) <- gsub("__","",colnames(out))
  return(out)
}


