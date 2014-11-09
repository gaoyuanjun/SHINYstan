library(shiny)

# load the helper functions
source("helper_functions/SHINYstan_helpers.R", local=TRUE)

# extract the content of the shiny_stan_object slots
source("server_files/extract_shiny_stan_object.R", local=TRUE)

# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
shinyServer(function(input, output) {

# reactive functions to get samples for a single parameter
  source("server_files/par_samps_reactive.R", local=TRUE)


# Output ------------------------------------------------------------------
# _________________________________________________________________________

#### TEXT: parameter name ####
  output$param_name <- renderText({
    input$param
  })

#### TABLE: summary stats (single parameter) ####
  source("server_files/parameter_summary_reactive.R", local = TRUE)
  output$parameter_summary_out <- renderTable({
    parameter_summary()
  }, include.rownames = FALSE, display = c("s","f","d",rep("f",5)))

#### PLOT: trace (single parameter) ####
  source("server_files/trace_plot_reactive.R", local = TRUE)
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
  source("server_files/density_plot_reactive.R", local = TRUE)
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
  source("server_files/contour_plot_reactive.R", local = TRUE)
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
  source("server_files/summary_stats_reactive.R", local = TRUE)
  output$all_summary_out <- renderDataTable({
    summary_stats()
  }, options = list(
    pageLength = 10,
    lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All')),
    columnDefs = list(list(targets = c(2:11) - 1, searchable = FALSE)),
    orderClasses = TRUE,
    scrollCollapse = TRUE,
    rowCallback = I(
      'function(row, data) {
        // color in red the names of parameters with rhats >= 1.1
        if (parseFloat(data[1]) >= 1.1)
         $("td:eq(0)", row).css("color", "red");
        if (parseFloat(data[1]) >= 1.1)
         $("td:eq(1)", row).css("color", "red");
      }'
    )))
  output$download_summary_stats <- downloadHandler(
    filename = 'shiny_stan_summary_stats.RData',
    content = function(file) {
      shiny_stan_summary_stats <- summary_stats()[,-1]
      save(shiny_stan_summary_stats, file = file)
    }
  )



#### PLOT: median, CI, and density (multiple parameters) ####
  source("server_files/multiparameter_plot_gg_reactive.R", local = TRUE)
  output$plot_param_vertical_out <- renderPlot({
    plot_param_vertical()
  }, width = 650, height = calc_height_param_plot)

  output$download_param_plot <- downloadHandler(
    filename = 'shiny_stan_param_plot.RData',
    content = function(file) {
      shiny_stan_param_plot <- plot_param_vertical()
      save(shiny_stan_param_plot, file = file)
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
