output$ui_autocorr_customize <- renderUI({
  fluidRow(
    column(5, selectizeInput("ac_params", label = h5("Select or enter parameter names"), width = '100%', choices = .make_param_list_with_groups(object), multiple = TRUE)),
    column(3, offset = 1, numericInput("ac_lags", label = h5("Number of lags"), value = 25, min = 0, step = 1)),
    column(2, offset = 1, checkboxInput("ac_flip", label = h5("Flip facets"), value = FALSE))
  )
})
