output$buttons_param_plot <- renderUI({
  fluidRow(
    column(4, downloadButton("download_param_plot", "Save ggplot2 object")),
    column(4, downloadButton("save_settings_param_plot", "Save appearance settings")),
    column(3, offset = 1, bsButton("param_plot_isolation", label = "Isolation view")),
    uiOutput("param_plot_modal")
  )
})

output$buttons_trace <- renderUI({
  fluidRow(
    column(4, downloadButton("download_trace", "Save ggplot2 object")),
    column(4, downloadButton("save_settings_trace", "Save appearance settings")),
    column(3, offset = 1, bsButton("trace_isolation", label = "Isolation view")),
    uiOutput("trace_plot_modal")
  )
})

output$buttons_contour <- renderUI({
  fluidRow(
    column(4, downloadButton("download_contour", "Save ggplot2 object")),
    column(4, downloadButton("save_settings_contour", "Save appearance settings")),
    column(3, offset = 1, bsButton("contour_isolation", label = "Isolation view")),
    uiOutput("contour_plot_modal")
  )
})

output$buttons_density <- renderUI({
  fluidRow(
    column(4, downloadButton("download_density", "Save ggplot2 object")),
    column(4, downloadButton("save_settings_density", "Save appearance settings")),
    column(3, offset = 1, bsToggleButton("density_isolation", label = "Isolation view")),
    uiOutput("density_plot_modal")
  )
})

