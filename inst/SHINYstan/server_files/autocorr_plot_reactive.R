# autocorr_plot
autocorr_plot <- reactive({
#   customize <- input$ac_customize
  do.call(".autocorr_plot", args = list(
    samps           = samps_all,
    params          = input$ac_params,
    all_param_names = param_names,
    lags            = input$ac_lags,
    flip            = input$ac_flip
  ))
})
