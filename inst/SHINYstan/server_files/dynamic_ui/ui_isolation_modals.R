output$param_plot_modal <- renderUI({
  bsModal("param_plot_isolation_modal", "Parameters plot", trigger = "param_plot_isolation",
          tags$div(class = "span12",
                   plotOutput("plot_param_vertical_out_isolation")
          )
  )
})

output$trace_plot_modal <- renderUI({
  param <- input$param
  param_label <- paste("Trace plot:", param)
  bsModal("trace_isolation_modal", title = param_label, trigger = "trace_isolation",
          tags$div(class = "span12",
                   plotOutput("trace_plot_out_isolation")
          )
  )
})

output$density_plot_modal <- renderUI({
  param <- input$param
  param_label <- paste("Posterior density:", param)
  bsModal("density_isolation_modal", title = param_label, trigger = "density_isolation",
          tags$div(class = "span12",
                   plotOutput("density_plot_out_isolation")
          )
  )
})

output$contour_plot_modal <- renderUI({
  param <- input$param
  param2 <- input$param2_contour
  param_label <- paste("Plot:", param, "vs.", param2)
  bsModal("contour_isolation_modal", title = param_label, trigger = "contour_isolation",
          tags$div(class = "span12",
                   plotOutput("contour_plot_out_isolation")
          )
  )
})
