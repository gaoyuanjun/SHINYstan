library(shiny)
library(shinyBS)

# load the helper functions
source("helper_functions/SHINYstan_helpers.R", local=TRUE)

# extract the content of the shiny_stan_object slots
source("server_files/utilities/extract_shiny_stan_object.R", local=TRUE)


# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
shinyServer(function(input, output, session) {

  # load reactive function for making sortable parameter list
  source("server_files/utilities/make_param_list_with_groups_sort.R", local=TRUE)

  # load all files for dynamic UIs
  ui_files <- list.files(path = "server_files/dynamic_ui", full.names = TRUE)
  for (f in ui_files) {
    source(f, local = TRUE)
  }

  # update Button appearances
  updateButtonGroup(session, "multiparam_sort", toggle = "radio", style = "default", size = "mini", disabled = FALSE, value = "k")
  updateButton(session, "btn_help_multiparam_sort", style = "inverse")

  # add tooltips
  addTooltip(session, id = "autocorr_isolation", title = "Open plot in modal window", placement = "left", trigger = "hover")




  # load reactive functions to get samples for a single parameter
  source("server_files/utilities/par_samps_reactive.R", local=TRUE)

  #### TEXT: parameter name ####
  output$param_name <- renderText({
    input$param
  })

  #### TABLE: summary stats (single parameter) ####
  source("server_files/outputs/parameter_summary_reactive.R", local = TRUE)
  output$parameter_summary_out <- renderTable({
    parameter_summary()
  }, include.rownames = FALSE, display = c("s","f","d",rep("f",5)))

  #### PLOT: trace (single parameter) ####
  source("server_files/outputs/trace_plot_reactive.R", local = TRUE)
  output$trace_plot_out <- renderPlot({
    trace_plot()
  })
  output$trace_plot_out_isolation <- renderPlot({
    trace_plot()
  })
  output$download_trace <- downloadHandler(
    filename = paste0('shinystan_trace_',input$param,'.RData'),
    content = function(file) {
      shiny_stan_trace <- trace_plot()
      comment(shinystan_trace) <- paste("Trace plot for parameter", input$param)
      save(shinystan_trace, file = file)
    }
  )
  source("server_files/save_settings/settings_trace_reactive.R", local = TRUE)
  output$save_settings_trace <- downloadHandler(
    filename = 'shinystan_settings_trace.RData',
    content = function(file) {
      shinystan_settings_trace <- save_settings_trace()
      save(shinystan_settings_trace, file = file)
    }
  )

  ### PLOT: density (single parameter) ####
  source("server_files/outputs/density_plot_reactive.R", local = TRUE)
  output$density_plot_out <- renderPlot({
    density_plot()
  })
  output$density_plot_out_isolation <- renderPlot({
    density_plot()
  })
  output$download_density <- downloadHandler(
    filename = 'shinystan_density.RData',
    content = function(file) {
      shinystan_density <- density_plot()
      save(shinystan_density, file = file)
    }
  )
  source("server_files/save_settings/settings_dens_reactive.R", local = TRUE)
  output$save_settings_density <- downloadHandler(
    filename = 'shinystan_settings_density.RData',
    content = function(file) {
      shinystan_settings_density <- save_settings_density()
      save(shinystan_settings_density, file = file)
    }
  )



  #### PLOT: contour (two parameters) ####
  source("server_files/outputs/contour_plot_reactive.R", local = TRUE)
  output$contour_plot_out <- renderPlot({
    contour_plot()
  })
  output$contour_plot_out_isolation <- renderPlot({
    contour_plot()
  })
  output$download_contour <- downloadHandler(
    filename = 'shinystan_bivariate.RData',
    content = function(file) {
      shinystan_bivariate <- contour_plot()
      save(shinystan_bivariate, file = file)
    }
  )
  source("server_files/save_settings/settings_contour_reactive.R", local = TRUE)
  output$save_settings_contour <- downloadHandler(
    filename = 'shinystan_settings_contour.RData',
    content = function(file) {
      shinystan_settings_contour <- save_settings_contour()
      save(shinystan_settings_contour, file = file)
    }
  )


  #### DATATABLE: summary stats (all parameters) ####
  source("server_files/outputs/summary_stats_reactive.R", local = TRUE)
  #   // color in red the names of parameters with rhats >= 1.1
  #     rowCallback = I(
  #       'function(row, data) {
  #         if (parseFloat(data["Rhat"]) >= 1.1)
  #          $("td:eq(0)", row).css("color", "red");
  #         if (parseFloat(data["Rhat"]) >= 1.1)
  #          $("td:eq(1)", row).css("color", "red");
  #       }'
  #     )
  output$all_summary_out <- renderDataTable({
    summary_stats()
  }, options = list(
    pageLength = 10,
    lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All')),
    orderClasses = TRUE,
    scrollCollapse = TRUE
  ))
  output$download_summary_stats <- downloadHandler(
    filename = 'shinystan_summary_stats.RData',
    content = function(file) {
      shinystan_summary_stats <- summary_stats()[,-1]
      save(shinystan_summary_stats, file = file)
    }
  )


  #### PLOT: median, CI, and density (multiple parameters) ####
  source("server_files/outputs/multiparameter_plot_gg_reactive.R", local = TRUE)
  source("server_files/outputs/multiparameter_plot_gg_slice_reactive.R", local = TRUE)
  output$plot_param_vertical_out <- renderPlot({
    plot_param_vertical()
  }, width = 650, height = calc_height_param_plot)
  output$plot_param_vertical_out_isolation <- renderPlot({
    plot_param_vertical()
  }, width = 650, height = calc_height_param_plot)

  output$download_param_plot <- downloadHandler(
    filename = 'shinystan_param_plot.RData',
    content = function(file) {
      shinystan_param_plot <- plot_param_vertical()
      save(shinystan_param_plot, file = file)
    }
  )
  output$plot_param_vertical_slice_out <- renderPlot({
    plot_param_vertical_slice()
  }, width = 650, height = calc_height_param_plot)

  output$download_param_plot_slice <- downloadHandler(
    filename = 'shinystan_param_plot.RData',
    content = function(file) {
      shinystan_param_plot <- plot_param_vertical_slice()
      save(shinystan_param_plot, file = file)
    }
  )

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


  #### TEXT: User's model notes ####
  observe({
    input$save_user_model_info
    isolate({
      if (input$user_model_info != "")
        shiny_stan_object@user_model_info <<- input$user_model_info
    })
  })
  output$user_text_saved <- renderText({
    input$save_user_model_info # take dependency on action button
    if (input$save_user_model_info != 0)
      print(paste("Saved:  ", format(Sys.time(), "%a %b %d %Y %X")))
  })


  #### PLOT: autocorrelation (multiple parameters) ####
  source("server_files/outputs/autocorr_plot_reactive.R", local = TRUE)
  output$autocorr_plot_out <- renderPlot({
    autocorr_plot()
  })
  output$autocorr_plot_out_isolation <- renderPlot({
    autocorr_plot()
  })
  output$download_autocorr <- downloadHandler(
    filename = paste0('shinystan_autocorr.RData'),
    content = function(file) {
      shinystan_autocorr <- autocorr_plot()
      save(shinystan_autocorr, file = file)
    }
  )



}) # End shinyServer
