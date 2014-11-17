# density_plot
density_plot <- reactive({

  if (input$param == "") {
    return()
  }

  customize <- input$dens_customize
  if (customize & is.null(input$dens_x_breaks)) {
    # delay until the customization inputs are ready
    return()
  }

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

