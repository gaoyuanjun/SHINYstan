# summary stats (single parameter)
parameter_summary <- reactive({
  do.call(".param_summary", args = list(
    param       = input$param,
    summary     = fit_summary
  ))
})
