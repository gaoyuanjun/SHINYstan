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
  parameter_summary <- reactive({
    do.call(".param_summary", args = list(
      param       = input$param,
      summary     = fit_summary
    ))
  })
  output$parameter_summary_out <- renderTable(parameter_summary(), 
                                              include.rownames = FALSE, 
                                              display = c("s","f","d",rep("f",5)))

#### PLOT: trace (single parameter) ####
  trace_plot <- reactive({
    zoom <- input$tracezoom
    customize <- input$trace_customize
    do.call(".param_trace", args = list(
      param       = input$param,
      dat         = par_samps_all(),
      chain       = input$trace_chain,
      warmup_val  = warmup_val,
      inc_warmup  = input$trace_warmup,
      palette     = input$trace_palette,
      rect        = ifelse(customize, input$trace_rect, "Samples"),
      rect_color  = ifelse(customize, input$trace_rect_color, "skyblue"),
      rect_alpha  = ifelse(customize, input$trace_rect_alpha, 0.15),
      x1          = ifelse(zoom, input$xzoom[1], NA),
      x2          = ifelse(zoom, input$xzoom[2], NA),
      y1          = ifelse(zoom, input$yzoom[1], NA),
      y2          = ifelse(zoom, input$yzoom[2], NA)
    ))
  })
  output$trace_plot_out <- renderPlot({
    trace_plot()
  })

  output$download_trace <- downloadHandler(
    filename = paste0('shiny_stan_trace_',input$param,'.RData'),
    content = function(file) {
      shiny_stan_trace <- trace_plot()
      comment(shiny_stan_trace) <- paste("Trace plot for parameter", input$param)
      save(shiny_stan_trace, file = file)
    }
  )

### PLOT: density (single parameter) ####
  density_plot <- reactive({
    customize <- input$dens_customize
    do.call(".param_dens", args = list(
      param       = input$param,
      dat         = par_samps_post_warmup(),
      chain       = input$dens_chain,
      fill_color  = ifelse(customize, input$dens_fill_color, "gray35"),
      line_color  = ifelse(customize, input$dens_line_color, "lightgray"), 
      point_est   = ifelse(customize, input$dens_point_est, "None"),
      CI          = ifelse(customize, input$dens_ci, "None"),
      y_breaks    = ifelse(customize, input$dens_y_breaks, "None"),
      x_breaks    = ifelse(customize, input$dens_x_breaks, "Some")
    )) 
  })
  output$density_plot_out <- renderPlot({
    density_plot()
  })
  output$download_density <- downloadHandler(
    filename = 'shiny_stan_density.RData',
    content = function(file) {
      shiny_stan_density <- density_plot()
      save(shiny_stan_density, file = file)
    }
  )

#### PLOT: contour (two parameters) ####
  contour_plot <- reactive({
    customize <- input$contour_customize
    type <- input$contour_type
    type_contour <- type == "Contour"
    type_point <- type == "Point"
    type_scatter <- type == "Scatter"
    if (customize & type_scatter & input$scatter_ellipse_lev != "None") {
      validate(need(input$param != input$param2_contour, 
                    "Please select a different 2nd parameter to use this option."))
    }
    do.call(".param_contour", args = list(
      samps       = samps_post_warmup,
      param       = input$param,
      param2      = input$param2_contour,
      type        = type,
      contour_ops = list(
        nBins       = ifelse(customize, input$contour_bins, 10),
        high_color  = ifelse(customize & type_contour, input$contour_high_color, 
                             ifelse(customize & type_point, input$point_high_color, "skyblue")),
        low_color   = ifelse(customize & type_contour, input$contour_low_color, 
                             ifelse(customize & type_point, input$point_low_color, "navyblue"))
        ),
      scatter_ops = list(
        pt_alpha    = ifelse(customize & type_scatter, input$scatter_pt_alpha, 0.35),
        pt_size     = ifelse(customize & type_scatter, input$scatter_pt_size, 2),
        pt_shape    = ifelse(customize & type_scatter, input$scatter_pt_shape, 1),
        pt_color    = ifelse(customize & type_scatter, input$scatter_pt_color, "black"),
        ci_lev      = ifelse(customize & type_scatter, input$scatter_ellipse_lev, "None"),
        ci_color    = ifelse(customize & type_scatter, input$scatter_ellipse_color, "black"),
        ci_lty      = ifelse(customize & type_scatter, input$scatter_ellipse_lty, 1),
        ci_lwd      = ifelse(customize & type_scatter, input$scatter_ellipse_lwd, 1),
        ci_alpha    = ifelse(customize & type_scatter, input$scatter_ellipse_alpha, 1)
        )
    ))
  })
  output$contour_plot_out <- renderPlot({
    contour_plot()
  })
  output$download_contour <- downloadHandler(
    filename = 'shiny_stan_bivariate.RData',
    content = function(file) {
      shiny_stan_bivariate <- contour_plot()
      save(shiny_stan_bivariate, file = file)
    }
  )

#### DATATABLE: summary stats (all parameters) ####
#   output$all_summary <- renderDataTable({
#     .all_summary(fit_summary)
#   }, callback='function(oTable) { new FixedHeader(oTable); }')

output$all_summary <- renderDataTable({
  .all_summary(fit_summary)
}, options = list(scrollY = 500, scrollX = 500))

#### PLOT: median, CI, and density (multiple parameters) ####
  calc_height_plot_param_vertical <- reactive({
    params <- input$params_to_plot
    params <- .update_params_with_groups(params, param_names)
    LL <- length(params)
    N <- ifelse(LL < 10, 10, LL)
    round(400*N/10)
  })

  plot_param_vertical <- reactive({
    customize <- input$param_plot_customize
    do.call(".plot_param_vertical_rhat", args = list(
      samps           = samps_post_warmup,
      params          = input$params_to_plot,
      all_param_names = param_names,
      CI.level        = input$CI_level/100,
      show.options    = input$show_options,
      rhat_values     = fit_summary[, "Rhat"],
      color_by_rhat   = ifelse(customize, input$param_plot_color_by_rhat, FALSE),
      point_est       = ifelse(customize, input$param_plot_point_est, "Median"),
      fill_color      = ifelse(customize, input$param_plot_fill_color, "gray35"),
      outline_color   = ifelse(customize, input$param_plot_outline_color, "black"),
      est_color       = ifelse(customize, input$param_plot_est_color, "black")
    ))
  })
  output$plot_param_vertical_out <- renderPlot({
    plot_param_vertical()
  }, height = calc_height_plot_param_vertical)

#### PLOT: Rhat (all parameters) ####
  output$rhat_plot <- renderPlot({
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


#### TEXT: Rhat Warnings #### 
  output$rhat_warnings <- renderText({
    .rhat_warnings(fit_summary)
  })


#### TEXT: SHINYstan credits ####
  output$SHINYstan_credits <- renderUI({
    jonah <- "Jonah Gabry"
    michael <- "Michael Andreae"
    yuanjun <- "Yuanjun Gao"
    dongying <- "Dongying Song"
    HTML(paste(jonah, michael, yuanjun, dongying, sep = '<br/>'))
  })

#### TEXT: SHINYstan credits ####
  output$Stan_credits <- renderText({
    "Stan Development Team, http://mc-stan.org"
  })

#### TEXT: User's model info ####
  observe({
    input$save_user_model_info
    isolate({
      if (input$user_model_info != "")
        shiny_stan_object@user_model_info <<- input$user_model_info
    })
  })


  
}) # End shinyServer
