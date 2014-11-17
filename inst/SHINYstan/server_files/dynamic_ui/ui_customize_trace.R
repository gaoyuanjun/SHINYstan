output$ui_trace_customize <- renderUI({
  # min and max for tracezoom value slider
  min_val <- floor(min(par_samps_all()))
  max_val <- ceiling(max(par_samps_all()))

  my_palette <- "Default"
  my_rect <- "Samples"
  my_rect_color <- "skyblue"
  my_rect_alpha <- 0.15

  if (input$user_trace_customize == TRUE) {
    ok <- exists("shinystan_settings_trace")
    validate(need(ok == TRUE, message = "Sorry, can't find any user trace plot settings."))
    user_trace <- shinystan_settings_trace
    my_palette <- user_trace$palette
    my_rect <- user_trace$rect
    my_rect_color <- user_trace$rect_color
    my_rect_alpha <- user_trace$rect_alpha
  }

  wellPanel(style = "background-color: #FFFFFF;",
            fluidRow(
              column(3, selectInput("trace_palette", h6("Color palette"), choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = my_palette, selectize=FALSE)),
              column(3, selectInput("trace_rect", h6("Shading"), choices = c("Samples", "Warmup","None"), selected = my_rect, selectize=FALSE)),
              column(3, selectInput("trace_rect_color", h6("Shading color"), choices = colors(), selected = my_rect_color, selectize=FALSE)),
              column(3, numericInput("trace_rect_alpha", h6("Shading opacity"), value = my_rect_alpha, min = 0, max = 1, step = 0.01))
            ),
            fluidRow(
              column(3, checkboxInput("trace_warmup", h5("Include warmup"), value = TRUE)),
              column(6, offset = 3, checkboxInput("tracezoom", label= h5("Enable TraceZoom (experimental)"), value = FALSE))
            ),
            conditionalPanel(condition = "input.tracezoom == true",
                             wellPanel(
                               # trace zoom options
                               fluidRow(
                                 # iterations slider
                                 column(3, offset = 1, sliderInput("xzoom", width = '100%', label = h6("Iterations"), min = 0, max = object@nIter, step = 1, value = c(0, object@nIter))),
                                 # value slider
                                 column(7, offset = 1, sliderInput("yzoom", width = '100%', label = h6("Value"), min = min_val, max = max_val, step = 0.01, value = c(min_val, max_val)))
                               )
                             )
            )
  )
})
