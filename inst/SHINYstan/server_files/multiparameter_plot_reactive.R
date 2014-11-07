# calc_height_param_plot
calc_height_param_plot <- reactive({
  params <- input$params_to_plot
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  N <- ifelse(LL < 10, 10, LL)
  round(400*N/10)
})

# param_plot_vertical
plot_param_vertical <- reactive({
  customize <- input$param_plot_customize
  do.call(".plot_param_vertical_rhat", args = list(
    samps           = samps_post_warmup,
    params          = input$params_to_plot,
    all_param_names = param_names,
    CI.level        = input$param_plot_ci_level/100,
    rhat_values     = fit_summary[, "Rhat"],
    show_density    = ifelse(customize, input$param_plot_show_density, FALSE),
    show_ci_line    = ifelse(customize, input$param_plot_show_ci_line, TRUE),
    color_by_rhat   = ifelse(customize, input$param_plot_color_by_rhat, FALSE),
    rhat_palette    = ifelse(customize, input$param_plot_rhat_palette, "Oranges"),
    point_est       = ifelse(customize, input$param_plot_point_est, "Median"),
    fill_color      = ifelse(customize, input$param_plot_fill_color, "gray35"),
    outline_color   = ifelse(customize, input$param_plot_outline_color, "black"),
    est_color       = ifelse(customize, input$param_plot_est_color, "black")
  ))
})
