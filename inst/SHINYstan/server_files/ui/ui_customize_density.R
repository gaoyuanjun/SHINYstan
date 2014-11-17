output$ui_density_customize <- renderUI({
  my_point_est <- "None"
  my_fill_color <- "gray35"
  my_line_color <- "lightgray"
  my_y_breaks <- "None"
  my_x_breaks <- "Some"
  my_CI <- "None"

  if (input$user_dens_customize == TRUE) {
    ok <- exists("shinystan_settings_density")
    validate(need(ok == TRUE, message = "Sorry, can't find any user density settings."))
    user_dens <- shinystan_settings_density
    my_point_est <- user_dens$point_est
    my_fill_color <- user_dens$fill_color
    my_line_color <- user_dens$line_color
    my_CI <- user_dens$CI
    my_y_breaks <- user_dens$y_breaks
    my_x_breaks <- user_dens$x_breaks
  }
  wellPanel(style = "background-color: #FFFFFF;",
            fluidRow(
              column(2, selectInput("dens_point_est", h6("Point est."), choices = c("None","Mean","Median","MAP"), selected = my_point_est, selectize = FALSE)),
              column(2, selectInput("dens_ci", h6("CI pct."), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = my_CI, selectize = FALSE)),
              column(2, selectInput("dens_fill_color", h6("Density color"), choices = colors(), selected = my_fill_color, selectize = FALSE)),
              column(2, selectInput("dens_line_color", h6("Line color"), choices = colors(), selected = my_line_color, selectize = FALSE)),
              column(2, selectInput("dens_y_breaks", h6("y breaks"), choices = c("None", "Some", "Many", "Too Many"), selected = my_y_breaks, selectize = FALSE)),
              column(2, selectInput("dens_x_breaks", h6("x breaks"), choices = c("None", "Some", "Many", "Too Many"), selected = my_x_breaks, selectize = FALSE))
            )
  )
})
