SHINYstan
=========

Shiny app for Stan models (work in progress). 


# Getting started:

1) Install "devtools" R package:

  install.packages("devtools")

2) Install the shinyBS R package:

  devtools::install_github("ebailey78/shinyBS")

3) Install the SHINYstan R package:

  devtools::install_github("jgabry/SHINYstan")

4) Load the SHINYstan package: 

  library(SHINYstan)

5) Use the launch_shinystan_demo() function to launch the demo or use launch_shinystan(object), replacing 'object' with a stanfit or shinystan object of your choice. 
