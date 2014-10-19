library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(rstan)

# load the helper functions
source("functions/SHINYstan_helpers.R", local=TRUE)

# give shiny_stan_object shorter name
object <- shiny_stan_object 


# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
shinyUI(
  fluidPage(
    verticalLayout(
      

# Title -------------------------------------------------------------------
# _________________________________________________________________________
fluidRow(
  #       img(src='stanlogo-main.png', align = "right"),
  column(8, h2(paste("Model:", object@model_name))),
  column(4, h1("SHINYstan"))
),

hr(),

# Main Panel --------------------------------------------------------------
# _________________________________________________________________________
mainPanel(width = 10,          
  tabsetPanel(type = "tabs", position = "above",
    
  #### TAB: Individual Parameters ####    
  tabPanel("Invidual Parameters",
    wellPanel(style = "background-color: #F0F8FF;",
    fluidRow(
      # select parameter
      column(5, selectizeInput(inputId = "param", label = "Select parameter:", choices = object@param_names, multiple = FALSE)),
      # summary stats 
      column(7, tableOutput("parameter_summary"))
    )),
    
    # display parameter name 
    h3(textOutput("param_name")),
  
    #### subTabs: parameter plots ####
    tabsetPanel(type = "tabs",position="left",
      
      ## Trace plot tab ##
      tabPanel("Trace", 
        wellPanel(
        # enter chain number
        numericInput("trace_chain", h6("Chain number (0 for all chains):"), min = 0, max = object@nChains, step = 1, value = 0),
        br(),br(),
        fluidRow(
          # enable trace zoom
          column(3,checkboxInput("tracezoom", label=h6("Enable TraceZoom"), value = FALSE)),
          # trace zoom description text
          column(9,helpText("TraceZoom allows you to interactively",
                            "control the range of iterations and values", 
                            "displayed in the trace plot."))
        ),
          conditionalPanel(condition = "input.tracezoom == true", 
            wellPanel(style = "background-color: #F7FCF5;",          
            # trace zoom options
            fluidRow(
              # iterations slider
              column(3, offset = 1, sliderInput("xzoom", width = '100%', label = h6("Iterations"), min = 0, max = object@nIter, value = c(0, object@nIter))),
              # value slider
              column(7, offset = 1, sliderInput("yzoom", width = '100%', label = h6("Value"), min = -25, max = 25, step = 0.01, value = c(-5, 5)))
            )
            )
          )
        ),
        # plot
        plotOutput("trace_plot")
      ),
      
      ## Density plot tab ##
      tabPanel("Density",       
        wellPanel(
        # enter chain number
        numericInput("dens_chain", h6("Chain number (0 for all chains):"), min = 0, max = object@nChains, step = 1, value = 0),
        checkboxInput("dens_customize", h6("Customize appearance"), value = FALSE),        
          conditionalPanel(condition = "input.dens_customize == true",
            wellPanel(style = "background-color: #F7FCF5;",
            fluidRow(
        # select estimates to show  
              column(3, selectInput("dens_point_est", h6("Point estimate"), choices = c("None","Mean","Median","MAP"), selected = "None")),
              column(3, selectInput("dens_ci", h6("Credible Interval"), choices = c("None" = "None", "50%" = 50, "80%" = 80, "95%" = 95), selected = "None")),
        # select colors    
              column(3, selectInput("dens_fill_color", h6("Density Color"), choices = colors(), selected = "black")),
              column(3, selectInput("dens_line_color", h6("Line Color"), choices = colors(), selected = "lightgray"))
            )
            )
          )
        ),
        plotOutput("density_plot")
      ),
      
      ## Contour plot tab ##
      tabPanel("Contour",
        wellPanel(
        # select 2nd parameter
        selectInput(inputId = "param2_contour", label = h6("Select a 2nd parameter:"), choices = object@param_names, multiple = FALSE),
        checkboxInput("contour_customize", h6("Customize appearance"), value = FALSE),
          conditionalPanel(condition = "input.contour_customize == true",
            wellPanel(style = "background-color: #F7FCF5;",
            fluidRow(
              #select type
              column(2, selectInput("contour_type", h6("Type"), choices = c("Point", "Contour"), selected = "Point")),
              # select colors    
              column(3, selectInput("contour_high_color", h6("High color"), choices = colors(), selected = "skyblue")),
              column(3, selectInput("contour_low_color", h6("Low color"), choices = colors(), selected = "navyblue")),
              # set binwidth
              column(4, numericInput("contour_bins", h6("Bins (if Type='Contour')"), value = 10, min = 1))
            )
            )
          )
        ),
        # plot
        plotOutput("contour_plot")
      )
    ) # END subTabs: parameter plots 
), # END TAB: Individual Parameters

  #### TAB: Model ####  
  tabPanel("Model",

    #### subTabs: model stats & plots ####
    tabsetPanel(type = "tabs", position = "above",
        
        # parameter plots tab       
        tabPanel("Multi-parameter plots",
        
          #### subTabs: multiparameter plots ####
          tabsetPanel(type = "pills",
            # median, CI, and density plot
            tabPanel("Parameter plot",
              wellPanel(
              fluidRow(
                # select parameters
                column(5, selectizeInput("params_to_plot", label = h6("Select or enter parameter names"), width = '100%', choices = object@param_names[-which(object@param_names=="lp__")], multiple = TRUE)),
                # slider for credible interval
                column(3, offset = 1, sliderInput("CI_level", h6("Credible Interval"), min = 50, max = 95, value = 50, step = 5)),
                # checkbox to show density
                column(2, offset = 1, checkboxGroupInput("show_options", label = h6("Display options"), choices = c("95% CI line" = "lines", Density = "density"), selected = "lines"))
              ),
              checkboxInput("param_plot_customize", h6("Customize appearance"), value = FALSE),
                conditionalPanel(condition = "input.param_plot_customize == true",
                  wellPanel(style = "background-color: #F7FCF5;",             
                  fluidRow(
                    # select colors    
                    column(3, selectInput("param_plot_fill_color", h6("Density/CI color"), choices = colors(), selected = "gray")),
                    column(3, selectInput("param_plot_outline_color", h6("Outline color"), choices = colors(), selected = "black")),
                    column(3, selectInput("param_plot_est_color", h6("Point estimate color"), choices = colors(), selected = "turquoise4")),
                    # select point estimate
                    column(3, selectInput("param_plot_point_est", h6("Point estimate value"), choices = c("Median", "Mean"), selected = "Median"))
                  )
                  )
                )
              ),
              # help text
              #helpText("Note: the thin lines are 95% credible intervals around the posterior median. The thick lines show the selected credible interval."),
              hr(),
              # plot
              plotOutput("plot_param_vertical")
            ),
            
            # Rhat plot tab
            tabPanel(withMathJax("\\(\\hat{R}\\) plot"),
              h4(withMathJax("Gelman & Rubin's  \\(\\hat{R}\\)  statistics")),
              helpText("Potential scale reduction factor"),
              hr(),
              plotOutput("rhatplot")
            )
          ) # END subTabs: multiparameter plots
        ),
        
        # Summary stats tab        
        tabPanel("Posterior summary statistics",
          br(),
          # data table
          dataTableOutput("all_summary")
        )
        
      ) # END subTabs: model stats & plots 
    ), # END TAB: model
    
    #### TAB: Sampler ####  
    tabPanel("Sampler Parameters", 
      h3("Average value of sampler parameters"),
      checkboxInput("sampler_warmup", label = h6("Include warmup period?"), value = TRUE),
      hr(),
      tableOutput("sampler_summary")
    ) # END TAB: sampler
    
) # END all tabs
) # END mainPanel
) # END verticalLayout
) # END fluidPage
) # END shinyUI