library(shiny)

# load the helper functions
source("functions/SHINYstan_helpers.R", local=TRUE)

# Extract the content of the shiny_stan_object slots
object <- shiny_stan_object 
samps_all <- object@samps_all
samps_post_warmup <- object@samps_post_warmup
sampler_params <- object@sampler_params
nIter <- object@nIter
warmup_val <- object@nWarmup
fit_summary <- object@summary
param_names <- object@param_names


# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
shinyServer(function(input, output) {
  
# Preliminaries -----------------------------------------------------------
# _________________________________________________________________________
  
  # reactive function to get samples for a single parameter
  par_samps_all <- reactive({    
    param <- input$param
    p <- which(param_names == param)
    samps_all[,,p]
  })
  par_samps_post_warmup <- reactive({    
    param <- input$param
    p <- which(param_names == param)
    samps_post_warmup[,,p]
  })
  
# Output ------------------------------------------------------------------
# _________________________________________________________________________  

#### TEXT: parameter name ####
  output$param_name <- renderText({
    input$param
  })

#### TABLE: summary stats (single parameter) ####
  output$parameter_summary <- renderTable({
    do.call(".param_summary", args = list(
      param       = input$param,
      r_e         = fit_summary[input$param,c("Rhat","n_eff")],
      dat         = par_samps_post_warmup(),
      warmup_val  = warmup_val
    ))
  }, include.rownames = FALSE, display = c("s","f","d",rep("f",5)))

#### PLOT: trace (single parameter) ####
  output$trace_plot <- renderPlot({
    zoom <- input$tracezoom
    do.call(".param_trace", args = list(
      param       = input$param,
      dat         = par_samps_all(),
      chain       = input$trace_chain,
      warmup_val  = warmup_val,
      x1          = ifelse(zoom, input$xzoom[1], NA),
      x2          = ifelse(zoom, input$xzoom[2], NA),
      y1          = ifelse(zoom, input$yzoom[1], NA),
      y2          = ifelse(zoom, input$yzoom[2], NA)
    ))
  })

#### PLOT: density (single parameter) ####
  output$density_plot <- renderPlot({
      customize <- input$dens_customize
      do.call(".param_dens", args = list(
        param       = input$param,
        dat         = par_samps_post_warmup(),
        chain       = input$dens_chain,
        warmup_val  = warmup_val,
        fill_color  = ifelse(customize, input$dens_fill_color, "black"),
        line_color  = ifelse(customize, input$dens_line_color, "lightgray"), 
        point_est = ifelse(customize, input$dens_point_est, "None"),
        CI = ifelse(customize, input$dens_ci, "None")
      )) 
  })

#### PLOT: contour (two parameters) ####
  output$contour_plot <- renderPlot({
    customize <- input$contour_customize
    do.call(".param_contour", args = list(
      samps       = samps_post_warmup,
      param       = input$param,
      param2      = input$param2_contour,
      type        = ifelse(customize, input$contour_type, "Point"),
      nBins       = ifelse(customize, input$contour_bins, 10),
      high_color  = ifelse(customize, input$contour_high_color, "skyblue"),
      low_color   = ifelse(customize, input$contour_low_color, "navyblue")
    ))
  })

#### DATATABLE: summary stats (all parameters) ####
  output$all_summary <- renderDataTable({
    .all_summary(fit_summary)
  }, options = list(scrollY = 500, scrollX = 500))


#### PLOT: median, CI, and density (multiple parameters) ####
  calc_height_plot_param_vertical <- reactive({
    params <- input$params_to_plot
    LL <- length(params)
    N <- ifelse(LL < 10, 10, LL)
    round(400*N/10)
  })

  output$plot_param_vertical <- renderPlot({
    customize <- input$param_plot_customize
  
    do.call(".plot_param_vertical", args = list(
      samps         = samps_post_warmup,
      params        = input$params_to_plot,
      CI.level      = input$CI_level/100,
      show.options  = input$show_options,
      point_est     = ifelse(customize, input$param_plot_point_est, "Median"),
      fill_color    = ifelse(customize, input$param_plot_fill_color, "gray"),
      outline_color = ifelse(customize, input$param_plot_outline_color, "black"),
      est_color  = ifelse(customize, input$param_plot_est_color, "turquoise4")
    ))
  }, height = calc_height_plot_param_vertical)


#### PLOT: Rhat (all parameters) ####
  output$rhatplot <- renderPlot({
    .rhat_plot(fit_summary)
  }, height = .calc_height_fixed(param_names))

#### TABLE: summary stats (sampler) ####
  output$sampler_summary <- renderTable({
    do.call(".sampler_summary", args = list(
      sampler_params  = sampler_params,
      inc_warmup      = input$sampler_warmup,
      warmup_val      = warmup_val
    ))
  })
  
}) # End shinyServer