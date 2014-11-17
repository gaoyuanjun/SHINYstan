# misc. functions --------------------------------------------------------
.in_range <- function(x, a, b) {
  x >= a & x <= b
}

# ggplot theme elements --------------------------------------------
fat_axis <- theme(axis.line = element_line(size = 1.75))
h_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.x = element_blank())
v_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.y = element_blank())
no_lgnd <- theme(legend.position = "none")


# make_param_list ------------------------------------------------------
.make_param_list <- function(object) {
  choices <- list()
  ll <- length(object@param_dims)
  choices[1:ll] <- ""
  names(choices) <- object@param_groups
  for(i in 1:ll) {
    if (length(object@param_dims[[i]]) == 0) {
      choices[[i]] <- list(object@param_groups[i])
    }
    else {
      temp <- paste0(object@param_groups[i],"\\[")
      choices[[i]] <- object@param_names[grep(temp, object@param_names)]
    }
  }
  choices
}

# make_param_list_with_groups ------------------------------------------------------
.make_param_list_with_groups <- function(object, sort_j = FALSE) {
  choices <- list()
  ll <- length(object@param_dims)
  LL <- sapply(1:ll, function(i) length(object@param_dims[[i]]))

  choices[1:ll] <- ""
  names(choices) <- object@param_groups
  for(i in 1:ll) {
    if (LL[i] == 0) {
      choices[[i]] <- list(object@param_groups[i])
    }
    else {
      group <- object@param_groups[i]
      temp <- paste0(group,"\\[")
      ch <- object@param_names[grep(temp, object@param_names)]

      if (sort_j == TRUE & LL[i] > 1) {
        # change sorting so to, e.g. "beta[1,1] beta[1,2] beta[2,1] beta[2,2]"
        # instead of "beta[1,1] beta[2,1] beta[1,2] beta[2,2]"
        ch <- gtools::mixedsort(ch)
      }

      # the next line avoids parameters whose names include the group name of a
      # different group of parameters being included in the latter group, e.g.
      # if we have b_bias[1], b_bias[2], bias[1], bias[2] then we want to avoid
      # bias[1] and bias[2] being included in the b_bias group
      ch <- ch[which(substr(ch, 1, nchar(group)) == group)]

      ch_out <- c(paste0(group,"_as_shiny_stan_group"), ch)
      names(ch_out) <- c(paste("ALL", group), ch)
      choices[[i]] <- ch_out
    }
  }

  choices
}



# param_trace -------------------------------------------------------------
.param_trace <- function(param, dat, warmup_val, inc_warmup, chain, palette,
                         rect, rect_color, rect_alpha, x1, x2, y1, y2) {
  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (!inc_warmup) dat <- subset(dat, iterations >= warmup_val)
  if (chain != 0) dat <- subset(dat, chains == paste0("chain:",chain))

  rect_xmin <- ifelse(rect == "Samples", Inf, -Inf)
  shading_rect <- annotate("rect", xmin = rect_xmin, xmax = warmup_val,
                           ymin = -Inf, ymax = Inf, fill = rect_color, alpha = rect_alpha)

  xy_labs <- labs(y = param, x = "Iteration\n Warmup | Samples")
  nclrs <- length(unique(dat$chains))
  if(palette == "Default") clrs <- scale_color_discrete()
  if(palette == "Gray") clrs <- scale_color_grey()
  if(palette == "Brewer (spectral)") clrs <- scale_color_brewer(palette = "Spectral")
  if(palette == "Rainbow") clrs <- scale_colour_manual(values = rainbow(nclrs))

  gg_trace <- ggplot(dat, aes(x = iterations, y = value, color = chains))
  gg_trace <- gg_trace + xy_labs + clrs + theme_classic() %+replace% (fat_axis + h_lines + no_lgnd)
  if (rect != "None") gg_trace <- gg_trace + shading_rect
  gg_trace <- gg_trace +  geom_line(size = 0.35)

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
.param_dens <- function(param, dat, chain,
                        fill_color = NULL, line_color = NULL,
                        point_est = "None", CI,
                        x_breaks = "Some", y_breaks = "None") {

  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (chain != 0) {
    dat <- subset(dat, chains == paste0("chain:",chain))
  }

  Mean <- mean(dat$value)
  Median <- median(dat$value)
  dens_dat <- with(density(dat$value), data.frame(x,y))
  MAP <- with(dens_dat, x[which.max(y)])

  fclr <- ifelse(is.null(fill_color), "black", fill_color)
  lclr <- ifelse(is.null(line_color), "lightgray", line_color)

  many_breaks <- function(x) pretty(x, n = 15)
  too_many_breaks <- function(x) pretty(x, n = 35)
  if(x_breaks == "None") x_scale <- scale_x_continuous(breaks = NULL)
  if(x_breaks == "Some") x_scale <- scale_x_continuous()
  if(x_breaks == "Many") x_scale <- scale_x_continuous(breaks = many_breaks)
  if(x_breaks == "Too Many") x_scale <- scale_x_continuous(breaks = too_many_breaks)
  if(y_breaks == "None") y_scale <- scale_y_continuous(breaks = NULL)
  if(y_breaks == "Some") y_scale <- scale_y_continuous()
  if(y_breaks == "Many") y_scale <- scale_y_continuous(breaks = many_breaks)
  if(y_breaks == "Too Many") y_scale <- scale_y_continuous(breaks = too_many_breaks)

  gg_dens <- ggplot(dens_dat, aes(x = x, ymax = y))
  gg_dens <- (gg_dens +
                labs(x = param, y = "") +
                x_scale + y_scale +
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
    lev <- (1 - as.numeric(CI))/2
    quant <- quantile(dat$value, probs = c(lev, 1 - lev))
    gg_dens <- (gg_dens +
                annotate("segment", x = quant, xend = quant, y = 0, yend = max(dens_dat$y), color = lclr, lty = rep(1:length(CI),2))
    )
  }
  gg_dens
}


# param_contour -----------------------------------------------------------
.param_contour <- function(samps, param, param2, type, ops) {
#                            contour_ops, scatter_ops) {

  if (type %in% c("Contour", "Point")) contour_ops <- ops
  if (type == "Scatter") scatter_ops <- ops

  shape_translator <- function(x) {
    shape <- ifelse(x >= 6, x + 9, x)
    shape
  }

  params <- c(param, param2)
  nParams <- 2
  nIter <- dim(samps)[1] * dim(samps)[2]
  samps_use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps_use) <- params
  dat <- data.frame(x = samps_use[,param], y = samps_use[,param2])
  g <- ggplot(dat, aes(x = x, y = y)) + labs(x = param, y = param2)
  if (type == "Point") {
    g <- (g +
            with(contour_ops, stat_density2d(geom="point", aes(size = ..density.., color = ..density..), contour = FALSE)) +
            with(contour_ops, scale_color_gradient(low = low_color, high = high_color)) +
            with(contour_ops, scale_size_continuous(range = c(0.25, 4.5)  )))
  }
  if (type == "Contour") {
    g <- (g +
            with(contour_ops, stat_density2d(geom="path", aes(color = ..level..), bins = nBins, contour = TRUE)) +
            with(contour_ops, scale_color_gradient(low = low_color, high = high_color)))
  }
  if (type == "Scatter") {
    g <- g + with(scatter_ops, geom_point(alpha = pt_alpha, size = pt_size, shape = shape_translator(pt_shape), color = pt_color))
    if (scatter_ops$ci_lev != "None") {
      g <- g + with(scatter_ops, stat_ellipse(level = as.numeric(ci_lev), color = ci_color, linetype = ci_lty, size = ci_lwd, alpha = ci_alpha))
    }
  }
  g + theme_classic() %+replace% (no_lgnd + fat_axis) + ggtitle(paste(param, "vs.", param2, "\n"))
}



# param_summary -----------------------------------------------------------
.param_summary <- function(param, summary) {
  out <- summary[param, c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")]
  outmat <- matrix(out, 1, length(out))
  colnames(outmat) <- names(out)
  rownames(outmat) <- NULL
  outmat
}


# all_summary -------------------------------------------------------------
.all_summary <- function(fit_summary, digits = 2, cols) {


#   order <- c("Rhat", "n_eff", "mean", "sd", "se_mean",
#             "2.5%", "25%", "50%", "75%", "97.5%")
  df <- as.data.frame(fit_summary[, cols])
  df <- round(df, digits)
  if ("n_eff" %in% cols) {
    df[,"n_eff"] <- round(df[,"n_eff"])
  }
  out <- cbind(Parameter = rownames(df), df)
  out
}


# update_params_with_groups -----------------------------------------------
  # update parameter selection for multi-parameter plot
.update_params_with_groups <- function(params, all_param_names) {
  as_group <- grep("_as_shiny_stan_group", params)
  if (length(as_group) == 0) {
    return(params)
  }

  make_group <- function(group_name) {
    temp <- paste0(group_name,"\\[")
    all_param_names[grep(temp, all_param_names)]
  }
  single_params <- params[-as_group]
  grouped_params <- params[as_group]
  groups <- gsub("_as_shiny_stan_group", "", grouped_params)
  groups <- sapply(groups, make_group)
  updated_params <- c(single_params, unlist(groups))
  updated_params
}


# plot_param_vertical_gg --------------------------------------------------
.plot_param_vertical_gg <- function(samps,
                                    params = NULL,
                                    all_param_names,
                                    show_density,
                                    show_ci_line,
                                    CI.level = 0.5,
                                    show.level = 0.95,
                                    point_est,
                                    rhat_values,
                                    color_by_rhat,
                                    rhat_palette,
                                    fill_color,
                                    outline_color,
                                    est_color) {

  params <- .update_params_with_groups(params, all_param_names)
  .e <- environment()
  dim.samps <- dim(samps) #nIter, nChain, nParam
  if(length(params) == 0) {
    params <- dimnames(samps)$parameters[1:min(10, dim.samps[3])]
  }
  params <- unique(params)
  Blues <- c("#C6DBEF", "#4292C6", "#08306B")
  Grays <- c("#D9D9D9", "#737373", "#000000")
  Greens <- c("#C7E9C0", "#41AB5D", "#00441B")
  Oranges <- c("#FDD0A2", "#F16913", "#7F2704")
  Purples <- c("#DADAEB", "#807DBA", "#3F007D")
  Reds <- c("#FCBBA1", "#EF3B2C", "#67000D")
  rhat_pal <- get(rhat_palette)
  rhat_id <- ifelse(rhat_values < 1.05, "A",
                    ifelse(rhat_values < 1.1, "B", "C"))
  rhat_id <- factor(rhat_id[params], levels = c("A","B", "C"), labels = c("<1.05", "<1.1", ">1.1"))
  rhat_colors <- scale_color_manual(name = bquote(hat(R)),
                                    values = rhat_pal,
                                    drop = FALSE)
  rhat_lgnd <- theme(legend.position = "top",
                     legend.title = element_text(size = 13, face = "bold"),
                     legend.text = element_text(size = 12))

  nParams <- length(params)
  nIter <- dim.samps[1] * dim.samps[2]
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params

  probs.use <- c(0.5 - show.level / 2,
                 0.5 - CI.level / 2,
                 0.5,
                 0.5 + CI.level / 2,
                 0.5 + show.level / 2)
  samps.quantile <- t(apply(samps.use, 2, quantile, probs = probs.use))
  y <- as.numeric(seq(nParams, 1, by = -1))

  xlim.use <- c(min(samps.quantile[,1]), max(samps.quantile[,5]))
  xrange <- diff(xlim.use)
  xlim.use[1] <- xlim.use[1] - 0.05 * xrange
  xlim.use[2] <- xlim.use[2] + 0.05 * xrange

  xy.df <- data.frame(params, y, samps.quantile)
  colnames(xy.df) <- c("params", "y", "ll", "l", "m", "h", "hh")
  if (point_est == "Mean") {
    xy.df$m <- apply(samps.use, 2, mean)
  }
  p.base <- ggplot(xy.df, environment = .e)
  p.name <- scale_y_continuous(breaks = y, labels = params)
  p.theme <- theme(axis.title=element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text=element_text(size=12),
                   axis.line=element_line(size = 1.75),
                   axis.line.y=element_blank(),
                   legend.position = "none",
                   panel.grid.major = element_line(size = 0.4))
  p.all <- p.base + p.name + theme_bw() + p.theme + xlim(xlim.use)

  if (show_ci_line | show_density) {
    p.ci <- geom_segment(aes(x = ll, xend = hh, y = y, yend = y),
                         colour = outline_color)
    p.all <- p.all + p.ci
  }
  if (show_density) {
    nPoint.den <- 512
    #plot density
    y.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    x.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,1],
                        to = samps.quantile[i,5],
                        n = nPoint.den)
      x.den[,i] <- d.temp$x
      y.max <- max(d.temp$y)
      y.den[,i] <- d.temp$y / y.max * 0.8 + y[i]
    }
    df.den <- data.frame(x = as.vector(x.den), y = as.vector(y.den),
                         name = rep(params, each = nPoint.den))
    p.den <- geom_line(data = df.den, aes(x = x, y = y, group = name),
                       color = outline_color)

    #shaded the confidence interval
    y.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    x.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,2],
                        to = samps.quantile[i,4],
                        n = nPoint.den)
      x.poly[,i] <- c(d.temp$x[1], as.vector(d.temp$x), d.temp$x[nPoint.den])
      y.max <- max(d.temp$y)
      y.poly[,i] <- as.vector(c(0, as.vector(d.temp$y) / y.max * 0.8, 0) + y[i])
    }
    df.poly <- data.frame(x = as.vector(x.poly), y = as.vector(y.poly),
                          name = rep(params, each = nPoint.den + 2))
    p.poly <- geom_polygon(data = df.poly, aes(x = x, y = y, group = name, fill = y))
    p.col <- scale_fill_gradient(low = fill_color, high = fill_color, guide = "none")

    #point estimator
    if (color_by_rhat) {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25, color = rhat_id),
                              size = 1.5)
      p.all + p.poly + p.den + p.col + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25),
                              colour = est_color,
                              size = 1.5)
      p.all + p.poly + p.den + p.col + p.point
    }

  } else {
    p.ci.2 <- geom_segment(aes(x = l, xend = h, y = y, yend = y),
                           colour = fill_color, size = 1.5)
    if (color_by_rhat) {
      p.point <- geom_point(aes(x = m, y = y, color = rhat_id), size = 3)
      p.all + p.ci.2 + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_point(aes(x = m, y = y), size = 3, colour = est_color)
      p.all + p.ci.2 + p.point
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
  my_clrs <- scale_color_manual(name = "", values = c("orange", "orangered", "maroon"),
                                labels = c(expression(hat(R) < 1.05), expression(hat(R) < 1.10),expression(hat(R) > 1.10)))
  axis_text <- theme(axis.text.y = element_text(face = "bold", color = "black"))
  gg_rhat <- ggplot(dat, aes(x = parameter, y = Rhat, ymax = Rhat, color = val))
  gg_rhat <- (gg_rhat + geom_pointrange(ymin = 1, size = .75) +
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


# rhat_warnings -----------------------------------------------------------
.rhat_warnings <- function(summary) {
  rhat <- summary[,"Rhat"]
  warn_params_1 <- names(which(rhat > 1.10))
  ll <- length(warn_params_1)
  if (ll == 0) {
    return("None")
  }
  return(paste0(warn_params_1, collapse = ", "))
}



# autocorr_plot -----------------------------------------------------------
.autocorr_plot <- function(samps, params = NULL, all_param_names, nChains,
                           lags = 25, flip = FALSE) {

  params <- .update_params_with_groups(params, all_param_names)
  if(length(params) == 0) {
    dim.samps <- dim(samps)
    params <- dimnames(samps)$parameters[1]
  }
  params <- unique(params)
  dat <- samps[,,params]
  dat <- melt(dat)


  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  nParams <- length(params)
  if (nParams == 1) {
    ac_dat <- ddply(dat, "chains", here(summarise),
                    ac = acf(value, lag.max = lags, plot = FALSE)$acf[,,1],
                    lag = 0:lags)
  }
  if (nParams > 1) {
    ac_dat <- ddply(dat, c("parameters", "chains"), here(summarise),
                    ac = acf(value, lag.max = lags, plot = FALSE)$acf[,,1],
                    lag = 0:lags)
  }
  ac_labs <- labs(x = "Lag", y = "Autocorrelation")
  strip_txt <- theme(strip.text = element_text(size = 12, face = "bold", color = "white"),
                     strip.background = element_rect(fill = "black"))
  ac_theme <- theme_classic() %+replace% (fat_axis + no_lgnd + strip_txt)
  y_scale <- scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0","","0.5","",""))

  gg_ac <- ggplot(ac_dat, aes(x = lag, y = ac, fill = factor(chains)))
  gg_ac <- (gg_ac +
              geom_bar(position = "identity", stat = "identity") +
              y_scale +
              ac_labs +
              ac_theme
  )

  if (nParams == 1) {
    gg_ac <- gg_ac + facet_wrap(~chains) + ggtitle(paste(params, "\n"))
  }
  if (nParams > 1) {
    if (flip == FALSE) {
      gg_ac <- gg_ac + facet_grid(parameters ~ chains)
    }
    if (flip == TRUE) {
      gg_ac <- gg_ac + facet_grid(chains ~ parameters)
    }
  }
  gg_ac
}





# plot_param_vertical_gg_slice --------------------------------------------------
.plot_param_vertical_gg_slice <- function(samps,
                                          params,
                                          param_dims,
                                          slice_txt,
                                          show_density,
                                          show_ci_line,
                                          CI.level = 0.5,
                                          show.level = 0.95,
                                          point_est,
                                          rhat_values,
                                          color_by_rhat,
                                          rhat_palette,
                                          fill_color,
                                          outline_color,
                                          est_color) {


  if (slice_txt == "") {
    params <- paste0(params,"[",1:min(10, param_dims[[params]]),"]")
  } else {
    params <- .update_params_with_slicing(params, slice_txt)
  }
  .e <- environment()
  dim.samps <- dim(samps) #nIter, nChain, nParam

  Blues <- c("#C6DBEF", "#4292C6", "#08306B")
  Grays <- c("#D9D9D9", "#737373", "#000000")
  Greens <- c("#C7E9C0", "#41AB5D", "#00441B")
  Oranges <- c("#FDD0A2", "#F16913", "#7F2704")
  Purples <- c("#DADAEB", "#807DBA", "#3F007D")
  Reds <- c("#FCBBA1", "#EF3B2C", "#67000D")
  rhat_pal <- get(rhat_palette)
  rhat_id <- ifelse(rhat_values < 1.05, "A",
                    ifelse(rhat_values < 1.1, "B", "C"))
  rhat_id <- factor(rhat_id[params], levels = c("A","B", "C"), labels = c("<1.05", "<1.1", ">1.1"))
  rhat_colors <- scale_color_manual(name = bquote(hat(R)),
                                    values = rhat_pal,
                                    drop = FALSE)
  rhat_lgnd <- theme(legend.position = "top",
                     legend.title = element_text(size = 13, face = "bold"),
                     legend.text = element_text(size = 12))

  nParams <- length(params)
  nIter <- dim.samps[1] * dim.samps[2]
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params

  probs.use <- c(0.5 - show.level / 2,
                 0.5 - CI.level / 2,
                 0.5,
                 0.5 + CI.level / 2,
                 0.5 + show.level / 2)
  samps.quantile <- t(apply(samps.use, 2, quantile, probs = probs.use))
  y <- as.numeric(seq(nParams, 1, by = -1))

  xlim.use <- c(min(samps.quantile[,1]), max(samps.quantile[,5]))
  xrange <- diff(xlim.use)
  xlim.use[1] <- xlim.use[1] - 0.05 * xrange
  xlim.use[2] <- xlim.use[2] + 0.05 * xrange

  xy.df <- data.frame(params, y, samps.quantile)
  colnames(xy.df) <- c("params", "y", "ll", "l", "m", "h", "hh")
  if (point_est == "Mean") {
    xy.df$m <- apply(samps.use, 2, mean)
  }
  p.base <- ggplot(xy.df, environment = .e)
  p.name <- scale_y_continuous(breaks = y, labels = params)
  p.theme <- theme(axis.title=element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text=element_text(size=12),
                   axis.line=element_line(size = 1.75),
                   axis.line.y=element_blank(),
                   legend.position = "none",
                   panel.grid.major = element_line(size = 0.4))
  p.all <- p.base + p.name + theme_bw() + p.theme + xlim(xlim.use)

  if (show_ci_line | show_density) {
    p.ci <- geom_segment(aes(x = ll, xend = hh, y = y, yend = y),
                         colour = outline_color)
    p.all <- p.all + p.ci
  }
  if (show_density) {
    nPoint.den <- 512
    #plot density
    y.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    x.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,1],
                        to = samps.quantile[i,5],
                        n = nPoint.den)
      x.den[,i] <- d.temp$x
      y.max <- max(d.temp$y)
      y.den[,i] <- d.temp$y / y.max * 0.8 + y[i]
    }
    df.den <- data.frame(x = as.vector(x.den), y = as.vector(y.den),
                         name = rep(params, each = nPoint.den))
    p.den <- geom_line(data = df.den, aes(x = x, y = y, group = name),
                       color = outline_color)

    #shaded the confidence interval
    y.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    x.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,2],
                        to = samps.quantile[i,4],
                        n = nPoint.den)
      x.poly[,i] <- c(d.temp$x[1], as.vector(d.temp$x), d.temp$x[nPoint.den])
      y.max <- max(d.temp$y)
      y.poly[,i] <- as.vector(c(0, as.vector(d.temp$y) / y.max * 0.8, 0) + y[i])
    }
    df.poly <- data.frame(x = as.vector(x.poly), y = as.vector(y.poly),
                          name = rep(params, each = nPoint.den + 2))
    p.poly <- geom_polygon(data = df.poly, aes(x = x, y = y, group = name, fill = y))
    p.col <- scale_fill_gradient(low = fill_color, high = fill_color, guide = "none")

    #point estimator
    if (color_by_rhat) {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25, color = rhat_id),
                              size = 1.5)
      p.all + p.poly + p.den + p.col + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25),
                              colour = est_color,
                              size = 1.5)
      p.all + p.poly + p.den + p.col + p.point
    }

  } else {
    p.ci.2 <- geom_segment(aes(x = l, xend = h, y = y, yend = y),
                           colour = fill_color, size = 1.5)
    if (color_by_rhat) {
      p.point <- geom_point(aes(x = m, y = y, color = rhat_id), size = 3)
      p.all + p.ci.2 + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_point(aes(x = m, y = y), size = 3, colour = est_color)
      p.all + p.ci.2 + p.point
    }
  }

}
.make_param_list_for_slicing <- function(object) {
  pd <- object@param_dims
  ll <- length(pd)
  choices <- rep(NA, ll)
  for(i in 1:ll) {
    if (length(pd[[i]]) == 1) {
      choices[i] <- names(pd[i])
    }
  }
  choices <- choices[!is.na(choices)]
  choices
}
# update_params_with_slicing  ---------------------------------------------
.update_params_with_slicing <- function(param, slice_txt) {
  slice <- parse(text = slice_txt)
  updated_params <- paste0(param,"[", eval(slice),"]")
  updated_params
}

