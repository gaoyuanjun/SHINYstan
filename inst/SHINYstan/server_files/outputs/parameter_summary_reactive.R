# summary stats (single parameter)
parameter_summary <- reactive({
  if (input$param == "") {
    return()
  }

  do.call(".param_summary", args = list(
    param       = input$param,
    summary     = fit_summary
  ))
})
