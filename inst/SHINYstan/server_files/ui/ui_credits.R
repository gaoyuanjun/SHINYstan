output$ui_credits <- renderUI({
  jonah <- "Jonah Gabry"
  michael <- "Michael Andreae"
  yuanjun <- "Yuanjun Gao"
  dongying <- "Dongying Song"
  HTML(paste(jonah, michael, yuanjun, dongying, sep = '<br/>'))
})
