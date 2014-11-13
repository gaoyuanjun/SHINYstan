library(shiny)
library(stringr)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)
library(rstan)

# load the helper functions
source("helper_functions/SHINYstan_helpers.R", local=TRUE)

# give shiny_stan_object shorter name
object <- shiny_stan_object


# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
shinyUI(
  fluidPage(theme = NULL,
    verticalLayout(


# Title -------------------------------------------------------------------
# _________________________________________________________________________
fluidRow(
  column(8, h2(paste("Model:", object@model_name))),
  column(4, h1("SHINYstan"))
),



# Main Panel --------------------------------------------------------------
# _________________________________________________________________________
mainPanel(width = 10,


  tags$style(type="text/css", "h1{color: #559e83;}"),
#   tags$style(type="text/css", "h5{color: #004444;}"),
  tags$style(type='text/css', ".well { padding-bottom: 5px; padding-top: 5px; }"),
  tags$style(type="text/css", "select.shiny-bound-input { font-size:10px; height:25px;}"),
# tags$style(type="text/css", "input.shiny-bound-input { font-size:10px; width: 45px; height:15px;}"),
  tags$style(type="text/css", "#trace_rect_alpha, #contour_bins, #scatter_pt_alpha, #scatter_pt_size, #scatter_pt_shape, #scatter_ellipse_lty, #scatter_ellipse_lwd, #scatter_ellipse_alpha { font-size:12px; width: 45px; height:15px;}"),
  tags$style(type="text/css", "#param2_contour, #contour_type { font-size:10px; height:25px;}"),
  tags$style(type="text/css", "#stats_digits, #ac_lags { width: 40px;}"),

  tabsetPanel(type = "tabs", position = "above",


    #### TAB: Model ####
    tabPanel(h4("  Main  "),

             #### subTabs ####
             tabsetPanel(type = "pills",
                         #### multiparameter plots ####
                         tabPanel(h5("Plots"),
                              tabsetPanel(
                                tabPanel("Model parameters",
                                  wellPanel(style = "background-color: #D3D3D3;",
                                            fluidRow(
                                              column(5, selectizeInput("params_to_plot", label = h5("Select or enter parameter names"), width = '100%', choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                              column(3, offset = 1, sliderInput("param_plot_ci_level", h5("Credible Interval"), min = 50, max = 95, value = 50, step = 5)),
                                              column(2, offset = 1,  checkboxInput("param_plot_customize", h5("Customize appearance"), value = FALSE))
                                            ),
#                                             checkboxInput("param_slicing", "Enable slicing", value = FALSE),
#                                             conditionalPanel(condition = "input.param_slicing == true",
#                                                                wellPanel(
#                                                                  fluidRow(
#                                                                    column(4, selectizeInput(inputId = "param_to_slice", label = "", choices = .make_param_list_for_slicing(object), multiple = FALSE)),
#                                                                    column(3, textInput("param_slice_txt", label = "", value = ""))
#                                                                   )
#                                                                )
#                                             ),
                                            conditionalPanel(condition = "input.param_plot_customize == true",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(2, checkboxInput("param_plot_show_density", h6("Show density"), value = FALSE)),
                                                                 column(3, checkboxInput("param_plot_show_ci_line", h6("Show 95% line"), value = TRUE)),
                                                                 column(3, checkboxInput("param_plot_color_by_rhat", h6("Color by Rhat Value"), FALSE))
                                                               ),
                                                               fluidRow(
                                                                 column(2, selectInput("param_plot_fill_color", h6("Density/CI color"), choices = colors(), selected = "gray35", selectize = FALSE)),
                                                                 column(2, selectInput("param_plot_outline_color", h6("Outline color"), choices = colors(), selected = "black", selectize = FALSE)),
                                                                 column(2, offset=1, selectInput("param_plot_point_est", h6("Point est. value"), choices = c("Median", "Mean"), selected = "Median", selectize = FALSE)),
                                                                 conditionalPanel(condition = "input.param_plot_color_by_rhat == false",
                                                                                  column(2, selectInput("param_plot_est_color", h6("Point est. color"), choices = colors(), selected = "black", selectize = FALSE))),
                                                                 conditionalPanel(condition = "input.param_plot_color_by_rhat == true",
                                                                                  column(2, selectInput("param_plot_rhat_palette", h6("Rhat colors"), choices = c("Blues", "Grays", "Greens", "Oranges", "Purples", "Reds"), selected = "Blues", selectize=FALSE)))
                                                               )
                                                             )
                                            )
                                  ),
                                  hr(),
                                  # plot
#                                   conditionalPanel(condition = "input.param_slicing == false",
                                    plotOutput("plot_param_vertical_out"),
                                    hr(),
                                    # export
                                    downloadButton("download_param_plot", "Save ggplot2 object (.RData)")
#                                   ),
#                                   conditionalPanel(condition = "input.param_slicing == true",
#                                                    plotOutput("plot_param_vertical_slice_out"),
#                                                    hr(),
#                                                    # export
#                                                    downloadButton("download_param_plot_slice", "Save ggplot2 object (.RData)")
#                                   )
                         ),
                        tabPanel("Markov chain autocorrelation",
                          wellPanel(style = "background-color: #D3D3D3;",
                            fluidRow(
                              column(5, selectizeInput("ac_params", label = h5("Select or enter parameter names"), width = '100%', choices = .make_param_list_with_groups(object), multiple = TRUE)),
                              column(3, offset = 1, numericInput("ac_lags", label = h5("Number of lags"), value = 25, min = 0, step = 1)),
                              column(2, offset = 1, checkboxInput("ac_flip", label = h5("Flip facets"), value = FALSE))
                            )
                          ),
                          plotOutput("autocorr_plot_out")
                        )
                    )
                  ),

                  #### summary stats ####
                  tabPanel(h5("Stats"),
                    tabsetPanel(
                      tabPanel("Posterior summary statistics",
                    sidebarPanel(style = "background-color: #D3D3D3;",
                      h4("Table options"),
                      hr(),
                      h5("Rounding"),
                      fluidRow(
                        column(2, numericInput("stats_digits", label = "", value = 2, min = 0, max = 7, step = 1)),
                        column(9, offset = 1, h6("decimal places"))
                      ),
                      checkboxGroupInput("stats_columns", label = h5("Columns"),
                                         choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "MCMC standard error" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                         selected = c("Rhat", "n_eff", "mean", "sd", "se_mean", "2.5%", "50%", "97.5%"))
                    ),
                    mainPanel(
                      # data table
                      dataTableOutput("all_summary_out"),
                      hr(),
                      # export
                      downloadButton("download_summary_stats", "Save summary stats (.RData)")
                    )
                  ),
                  tabPanel("HMC Sampler Parameters (Stan models only)",
                           h3("Average value of sampler parameters"),
                           checkboxInput("sampler_warmup", label = h6("Include warmup period?"), value = TRUE),
                           hr(),
                           tableOutput("sampler_summary")
                  )
                )
              )
             ) # END subTabs: model stats & plots
    ), # END TAB: model

    #### TAB: Individual Parameters ####
    tabPanel(h4("Explore Parameters"),
             wellPanel(style = "background-color: #F0F8FF; padding-top: 10px;",
                       fluidRow(
                         column(2, h4("Select parameter")),
                         column(3, selectizeInput(inputId = "param", label = "", choices = .make_param_list(object), multiple = FALSE)),
                         # summary stats
                         column(6, offset = 1, tableOutput("parameter_summary_out"))
                       )),

             # display parameter name
             h3(textOutput("param_name")),

             #### subTabs: parameter plots ####
             tabsetPanel(type = "tabs",position="left",

                         ## Trace plot tab ##
                         tabPanel("Trace",
                                  plotOutput("trace_plot_out"),
                                  br(),
                                  wellPanel(style = "background-color: #D3D3D3;",
                                            fluidRow(
                                              column(1, numericInput("trace_chain", label = "", min = 0, max = object@nChains, step = 1, value = 0)),
                                              column(3, h5("Chain (0 = all chains)")),
                                              column(3, checkboxInput("trace_warmup", h5("Include warmup"), value = TRUE)),
                                              column(4, offset = 1, checkboxInput("trace_customize", h5("Customize appearance"), value = FALSE))
                                            ),
                                            conditionalPanel(condition = "input.trace_customize == true",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(3, selectInput("trace_palette", h6("Color palette"), choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = "ggplot Default", selectize=F)),
                                                                 column(3, selectInput("trace_rect", h6("Shading"), choices = c("Samples", "Warmup","None"), selected = "Samples", selectize=F)),
                                                                 column(3, selectInput("trace_rect_color", h6("Shading color"), choices = colors(), selected = "skyblue", selectize=F)),
                                                                 column(3, numericInput("trace_rect_alpha", h6("Shading opacity"), value = 0.15, min = 0, max = 1, step = 0.01))
                                                               ),
                                                               fluidRow(
                                                                 # enable trace zoom
                                                                 column(3,checkboxInput("tracezoom", label=h6("Enable TraceZoom"), value = FALSE)),
                                                                 column(9,helpText("TraceZoom allows you to interactively",
                                                                                   "control the range of iterations and values",
                                                                                   "displayed in the trace plot."))
                                                               ),
                                                               conditionalPanel(condition = "input.tracezoom == true",
                                                                                wellPanel(
                                                                                  # trace zoom options
                                                                                  fluidRow(
                                                                                    # iterations slider
                                                                                    column(3, offset = 1, sliderInput("xzoom", width = '100%', label = h6("Iterations"), min = 0, max = object@nIter, value = c(0, object@nIter))),
                                                                                    # value slider
                                                                                    column(7, offset = 1, sliderInput("yzoom", width = '100%', label = h6("Value"), min = -25, max = 25, step = 0.01, value = c(-5, 5)))
                                                                                  )
                                                                                )
                                                               )
                                                             )
                                            )
                                  ),
                                  hr(),
                                  # export
                                  downloadButton("download_trace", "Save ggplot2 object (.RData)")
                         ),

                         ## Density plot tab ##
                         tabPanel("Density",
                                  br(),
                                  plotOutput("density_plot_out"),
                                  wellPanel(style = "background-color: #D3D3D3;",
                                            fluidRow(
                                              column(1, numericInput("dens_chain", label = "", min = 0, max = object@nChains, step = 1, value = 0)),
                                              column(3, h5("Chain (0 = all chains)")),
                                              column(4, offset = 1, checkboxInput("dens_customize", h5("Customize appearance"), value = FALSE))
                                            ),
                                            conditionalPanel(condition = "input.dens_customize == true",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(2, selectInput("dens_point_est", h6("Point est."), choices = c("None","Mean","Median","MAP"), selected = "None", selectize = FALSE)),
                                                                 column(2, selectInput("dens_ci", h6("CI pct."), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = "None", selectize = FALSE)),
                                                                 column(2, selectInput("dens_fill_color", h6("Density color"), choices = colors(), selected = "gray35", selectize = FALSE)),
                                                                 column(2, selectInput("dens_line_color", h6("Line color"), choices = colors(), selected = "lightgray", selectize = FALSE)),
                                                                 column(2, selectInput("dens_y_breaks", h6("y breaks"), choices = c("None", "Some", "Many", "Too Many"), selected = "None", selectize = FALSE)),
                                                                 column(2, selectInput("dens_x_breaks", h6("x breaks"), choices = c("None", "Some", "Many", "Too Many"), selected = "Some", selectize = FALSE))
                                                               )
                                                             )
                                            )
                                  ),
                                  hr(),
                                  # export
                                  downloadButton("download_density", "Save ggplot2 object (.RData)")
                         ),

                         ## Contour plot tab ##
                         tabPanel("Bivariate",
                                  # select 2nd parameter
                                  fluidRow(
                                    column(3, h5("Second parameter")),
                                    column(6, selectizeInput(inputId = "param2_contour", label = "", choices = rev(.make_param_list(object)), multiple = FALSE))
                                  ),
                                  # plot
                                  plotOutput("contour_plot_out"),
                                  wellPanel(style = "background-color: #D3D3D3;",
                                            fluidRow(
                                              column(1, h5("Style")),
                                              column(3, selectInput("contour_type", label = "", choices = c("Scatter", "Contour", "Point"), selected = "Scatter")),
                                              column(4, offset=1, checkboxInput("contour_customize", h5("Customize appearance"), value = FALSE))
                                            ),
                                            conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Contour')",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(3, selectInput("contour_high_color", h6("High color"), choices = colors(), selected = "skyblue", selectize = FALSE)),
                                                                 column(3, selectInput("contour_low_color", h6("Low color"), choices = colors(), selected = "navyblue", selectize = FALSE)),
                                                                 column(2, numericInput("contour_bins", h6("Bins"), value = 10, min = 1))
                                                               )
                                                             )
                                            ),
                                            conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Point')",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(3, selectInput("point_high_color", h6("High color"), choices = colors(), selected = "skyblue", selectize = FALSE)),
                                                                 column(3, selectInput("point_low_color", h6("Low color"), choices = colors(), selected = "navyblue", selectize = FALSE))
                                                               )
                                                             )
                                            ),
                                            conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Scatter')",
                                                             wellPanel(
                                                               fluidRow(
                                                                 column(2, offset=8, h6("Show Ellipse")),
                                                                 column(2, selectInput("scatter_ellipse_lev", label="", choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99), selected = "None", selectize = FALSE))
                                                               ),
                                                               fluidRow(
                                                                 # point options
                                                                 column(2, selectInput("scatter_pt_color", h6("Point Color"), choices = colors(), selected = "black", selectize = FALSE)),
                                                                 column(1, numericInput("scatter_pt_size", h6("Size"), value = 2, min = 0, max = 10, step = 0.5)),
                                                                 column(1, numericInput("scatter_pt_shape", h6("Shape"), value = 1, min = 1, max = 10, step = 1)),
                                                                 column(2, numericInput("scatter_pt_alpha", h6("Opacity"), value = 0.35, min = 0, max = 1, step = 0.01)),
                                                                 column(2, selectInput("scatter_ellipse_color", h6("Ellipse Color"), choices = colors(), selected = "black", selectize = FALSE)),
                                                                 column(1, numericInput("scatter_ellipse_lwd", h6("Size"), value = 1, min = 0, max = 5, step = 0.5)),
                                                                 column(1, numericInput("scatter_ellipse_lty", h6("Shape"), value = 1, min = 1, max = 6, step = 1)),
                                                                 column(2, numericInput("scatter_ellipse_alpha", h6("Opacity"), value = 1, min = 0, max = 1, step = 0.01))
                                                               )
                                                             )
                                            )
                                  ),
                                  hr(),
                                  # export
                                  downloadButton("download_contour", "Save ggplot2 object (.RData)")
                         )
             ) # END subTabs: parameter plots
    ), # END TAB: Individual Parameters

#     #### TAB: Sampler ####
#     tabPanel("Sampler Parameters",
#              h3("Average value of sampler parameters"),
#              checkboxInput("sampler_warmup", label = h6("Include warmup period?"), value = TRUE),
#              hr(),
#              tableOutput("sampler_summary")
#     ), # END TAB: sampler



    #### TAB: Warnings ####
#     tabPanel(h4("Quick Warnings"),
#              helpText("This tab displays things that seem to have gone wrong during sampling.", "REPLACE THIS TEXT WITH SOMETHING BETTER"),
#              br(),br(),
#              h4("The following parameters have Rhat values above 1.1:"),
#              br(),
#              textOutput("rhat_warnings")
#     ), # END TAB: warnings
#


    #### TAB: Notes ####
    tabPanel(h4("User Notes"),
             br(),
             helpText("Use this space to store notes about your model. ",
                      "The text will be saved in the user_model_info slot of",
                      "your shinystan object and displayed here each time SHINYstan",
                      "is launched for this object."),
             br(),
             tags$textarea(id="user_model_info", style = "width: 500px;", rows=10, cols=60, shiny_stan_object@user_model_info),
             br(),
             fluidRow(
              column(3, actionButton("save_user_model_info", label = "Save Changes")),
              column(8, offset = 1, textOutput("user_text_saved"))
              ),
             hr()
             #       h6("Why use this feature?"),
             #       helpText("If you want to allow other users to explore your model with",
             #                "SHINYstan, you can send them your shiny_stan_object and they",
             #                "will see any comments you've saved.")
    ), # END TAB: notes



    #### TAB: Credits ####
    tabPanel(h4(style = "color: #D3D3D3;", "Credits"),
             h3("SHINYstan"),
             htmlOutput("SHINYstan_credits"),
             br(),
             h3("Stan & RStan"),
             a("Stan Development Team", href="http://mc-stan.org/team.html")
    ) # END TAB: credits


) # END all tabs
) # END mainPanel
) # END verticalLayout
) # END fluidPage
) # END shinyUI
