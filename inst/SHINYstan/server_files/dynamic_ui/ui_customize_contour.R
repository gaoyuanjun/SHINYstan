output$ui_contour_customize_scatter <- renderUI({
  my_ellipse_lev <- "None"
  my_pt_alpha <- 0.35
  my_pt_size     <- 2
  my_pt_shape    <- 1
  my_pt_color    <- "firebrick"
  my_ci_color    <- "black"
  my_ci_lty      <- 1
  my_ci_lwd      <- 1
  my_ci_alpha    <- 1

  if (input$user_contour_customize == TRUE) {
    ok <- exists("shinystan_settings_contour")
    validate(need(ok == TRUE, message = "Sorry, can't find any user bivariate plot settings."))
    user_contour <- shinystan_settings_contour
    ops <- user_contour[["ops"]]

    my_ellipse_lev <- ops$ci_lev
    my_pt_alpha    <- ops$pt_alpha
    my_pt_size     <- ops$pt_size
    my_pt_shape    <- ops$pt_shape
    my_pt_color    <- ops$pt_color
    my_ci_color    <- ops$ci_color
    my_ci_lty      <- ops$ci_lty
    my_ci_lwd      <- ops$ci_lwd
    my_ci_alpha    <- ops$ci_alpha
  }

  wellPanel(style = "background-color: #FFFFFF;",
            fluidRow(
              column(2, offset = 6, h5("Show ellipse")),
              column(3, selectizeInput("scatter_ellipse_lev", label="", choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99), selected = my_ellipse_lev))
            ),
            fluidRow(
              column(2, selectInput("scatter_pt_color", h6("Point Color"), choices = colors(), selected = my_pt_color, selectize = FALSE)),
              column(1, numericInput("scatter_pt_size", h6("Size"), value = my_pt_size, min = 0, max = 10, step = 0.5)),
              column(1, numericInput("scatter_pt_shape", h6("Shape"), value = my_pt_shape, min = 1, max = 10, step = 1)),
              column(2, numericInput("scatter_pt_alpha", h6("Opacity"), value = my_pt_alpha, min = 0, max = 1, step = 0.01)),
              column(2, selectInput("scatter_ellipse_color", h6("Ellipse Color"), choices = colors(), selected = my_ci_color, selectize = FALSE)),
              column(1, numericInput("scatter_ellipse_lwd", h6("Size"), value = my_ci_lwd, min = 0, max = 5, step = 0.5)),
              column(1, numericInput("scatter_ellipse_lty", h6("Shape"), value = my_ci_lty, min = 1, max = 6, step = 1)),
              column(2, numericInput("scatter_ellipse_alpha", h6("Opacity"), value = my_ci_alpha, min = 0, max = 1, step = 0.01))
            )
  )
})



output$ui_contour_customize_contour <- renderUI({

  my_nBins <- 10
  my_high_color <- "skyblue"
  my_low_color <- "navyblue"

  if (input$user_contour_customize == TRUE) {
    ok <- exists("shinystan_settings_contour")
    validate(need(ok == TRUE, message = "Sorry, can't find any user bivariate plot settings."))
    user_contour <- shinystan_settings_contour
    ops <- user_contour$ops
    my_nBins <- ops$nBins
    my_high_color <- ops$high_color
    my_low_color <- ops$low_color
  }
  wellPanel(style = "background-color: #FFFFFF;",
            fluidRow(
              column(3, selectInput("contour_high_color", h6("High color"), choices = colors(), selected = my_high_color, selectize = FALSE)),
              column(3, selectInput("contour_low_color", h6("Low color"), choices = colors(), selected = my_low_color, selectize = FALSE)),
              column(2, numericInput("contour_bins", h6("Bins"), value = my_nBins, min = 1))
            )
  )

})

output$ui_contour_customize_point <- renderUI({

  my_high_color <- "skyblue"
  my_low_color <- "navyblue"
  if (input$user_contour_customize == TRUE) {
    ok <- exists("shinystan_settings_contour")
    validate(need(ok == TRUE, message = "Sorry, can't find any user bivariate plot settings."))
    user_contour <- shinystan_settings_contour
    ops <- user_contour$ops
    my_high_color <- ops$high_color
    my_low_color <- ops$low_color
  }
  wellPanel(style = "background-color: #FFFFFF;",
            fluidRow(
              column(3, selectInput("point_high_color", h6("High color"), choices = colors(), selected = my_high_color, selectize = FALSE)),
              column(3, selectInput("point_low_color", h6("Low color"), choices = colors(), selected = my_low_color, selectize = FALSE))
            )
  )
})


