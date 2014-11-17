# contour_plot
contour_plot <- reactive({

  if (input$param == "") {
    return()
  }

  type <- input$contour_type
  type_contour <- type == "Contour"
  type_point <- type == "Point"
  type_scatter <- type == "Scatter"
  customize <- input$contour_customize

  if (customize & type_scatter & is.null(input$scatter_ellipse_lev)) {
    return()
  }
  if (customize & type_contour & is.null(input$contour_bins)) {
    return()
  }
  if (customize & type_point & is.null(input$point_high_color)) {
    return()
  }


  if (customize & type_scatter & input$scatter_ellipse_lev != "None") {
    validate(need(input$param != input$param2_contour,
                  "For this option the x and y can't be the same parameter."))
  }
  if (type_contour | type_point) {
    ops <- list(
      nBins       = ifelse(customize, input$contour_bins, 10),
      high_color  = ifelse(customize & type_contour, input$contour_high_color,
                           ifelse(customize & type_point, input$point_high_color, "skyblue")),
      low_color   = ifelse(customize & type_contour, input$contour_low_color,
                           ifelse(customize & type_point, input$point_low_color, "navyblue"))
    )
  }
  if (type_scatter) {
    ops <- list(
      pt_alpha    = ifelse(customize & type_scatter, input$scatter_pt_alpha, 0.35),
      pt_size     = ifelse(customize & type_scatter, input$scatter_pt_size, 2),
      pt_shape    = ifelse(customize & type_scatter, input$scatter_pt_shape, 1),
      pt_color    = ifelse(customize & type_scatter, input$scatter_pt_color, "black"),
      ci_lev      = ifelse(customize & type_scatter, input$scatter_ellipse_lev, "None"),
      ci_color    = ifelse(customize & type_scatter, input$scatter_ellipse_color, "black"),
      ci_lty      = ifelse(customize & type_scatter, input$scatter_ellipse_lty, 1),
      ci_lwd      = ifelse(customize & type_scatter, input$scatter_ellipse_lwd, 1),
      ci_alpha    = ifelse(customize & type_scatter, input$scatter_ellipse_alpha, 1)
    )
  }
  do.call(".param_contour", args = list(
    samps       = samps_post_warmup,
    param       = input$param,
    param2      = input$param2_contour,
    type        = type,
    ops         = ops
#     contour_ops = list(
#       nBins       = ifelse(customize, input$contour_bins, 10),
#       high_color  = ifelse(customize & type_contour, input$contour_high_color,
#                            ifelse(customize & type_point, input$point_high_color, "skyblue")),
#       low_color   = ifelse(customize & type_contour, input$contour_low_color,
#                            ifelse(customize & type_point, input$point_low_color, "navyblue"))
#     ),
#     scatter_ops = list(
#       pt_alpha    = ifelse(customize & type_scatter, input$scatter_pt_alpha, 0.35),
#       pt_size     = ifelse(customize & type_scatter, input$scatter_pt_size, 2),
#       pt_shape    = ifelse(customize & type_scatter, input$scatter_pt_shape, 1),
#       pt_color    = ifelse(customize & type_scatter, input$scatter_pt_color, "black"),
#       ci_lev      = ifelse(customize & type_scatter, input$scatter_ellipse_lev, "None"),
#       ci_color    = ifelse(customize & type_scatter, input$scatter_ellipse_color, "black"),
#       ci_lty      = ifelse(customize & type_scatter, input$scatter_ellipse_lty, 1),
#       ci_lwd      = ifelse(customize & type_scatter, input$scatter_ellipse_lwd, 1),
#       ci_alpha    = ifelse(customize & type_scatter, input$scatter_ellipse_alpha, 1)
#     )
  ))
})
