# autocorr_plot
autocorr_plot <- reactive({
  if (is.null(input$ac_lags)) {
    return()
  }
  if (!is.numeric(input$ac_lags) | input$ac_lags < 0) {
    return(last_plot())
  }

  do.call(".autocorr_plot", args = list(
    samps           = samps_all,
    params          = input$ac_params,
    all_param_names = param_names,
    lags            = input$ac_lags,
    flip            = input$ac_flip,
    nChains         = nChains
  ))
})
