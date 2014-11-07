# trace_plot
trace_plot <- reactive({
  zoom <- input$tracezoom
  customize <- input$trace_customize
  do.call(".param_trace", args = list(
    param       = input$param,
    dat         = par_samps_all(),
    chain       = input$trace_chain,
    warmup_val  = warmup_val,
    inc_warmup  = input$trace_warmup,
    palette     = input$trace_palette,
    rect        = ifelse(customize, input$trace_rect, "Samples"),
    rect_color  = ifelse(customize, input$trace_rect_color, "skyblue"),
    rect_alpha  = ifelse(customize, input$trace_rect_alpha, 0.15),
    x1          = ifelse(zoom, input$xzoom[1], NA),
    x2          = ifelse(zoom, input$xzoom[2], NA),
    y1          = ifelse(zoom, input$yzoom[1], NA),
    y2          = ifelse(zoom, input$yzoom[2], NA)
  ))
})
