pkgs <- c("shiny", "shinyBS", "ggplot2", "grid", "gtools", "plyr", "reshape2")
SHINYstan:::Librarian(pkgs)

# load the helper functions
source("helper_functions/SHINYstan_helpers.R", local = TRUE)

# give shiny_stan_object shorter name
object <- shiny_stan_object


# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
shinyUI(
  fluidPage(theme = NULL,
#   fluidPage(theme = "flatly.css",
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
                        withMathJax(),


                        #### tags ####
                        #   tags$style( # supress R's red error messages
                        #     type="text/css",
                        #     ".shiny-output-error { visibility: hidden; }",
                        #     ".shiny-output-error:before { visibility: hidden; }"
                        #   ),

#                         tags$style(type="text/css", "h1{color: #559e83;}"),
                        tags$style(type="text/css", "h1{color: #83adb5;}"),
                        tags$style(type="text/css", "h5{color: #428bca;}"),
                        tags$style(type='text/css', ".well { padding-bottom: 5px; padding-top: 5px; }"),
                        tags$style(type="text/css", "select.shiny-bound-input { font-size:10px; height:25px;}"),
                        tags$style(type="text/css", "#trace_rect_alpha, #contour_bins, #scatter_pt_alpha, #scatter_pt_size, #scatter_pt_shape, #scatter_ellipse_lty, #scatter_ellipse_lwd, #scatter_ellipse_alpha { font-size:12px; width: 45px; height:15px;}"),
                        tags$style(type="text/css", "#param2_contour, #contour_type { font-size:10px; height:25px;}"),
                        tags$style(type="text/css", "#stats_digits, #ac_lags { width: 40px;}"),
                        tags$style(type="text/css", "#save_settings_trace, #save_settings_density, #save_settings_contour, #save_settings_param_plot {border-color: #428bca; font-size:12px;}"),
                        tags$style(type="text/css", "#download_param_plot, #download_trace, #download_contour, #download_density, #download_summary_stats {border-color: #5e3c58; font-size: 12px;}"),
                        tags$style(type="text/css", "#trace_isolation, #density_isolation, #contour_isolation, #autocorr_isolation, #param_plot_isolation {font-size: 12px;}"),
                        tags$style(type="text/css", ".modal-header {background-color: #83adb5; border-radius: 5px;}"),
                        #                         tags$style(type="text/css", ".modal-body {font-size: 15px; filter: alpha(opacity=20); opacity: .7;}"),
                        tags$style(type="text/css", ".modal-backdrop {background-color: #C6DBEF; filter: alpha(opacity=100);}"),
                        tags$style(type="text/css", ".accordion-heading {background-color: transparent;}"),
                        tags$style(type="text/css", ".accordion-inner {background-color: #ecf3f9;}"),
                        tags$style(type="text/css", ".table-bordered {border-color: #428bca;}"),
                        tags$style(type="text/css", ".nav-pills a:hover{color: #214565;}"),
                        tags$style(type="text/css", ".nav-pills a{font-size: 18px; font-weight: 900;  }"),
                        tags$style(type="text/css", ".nav-pills li.active a{font-size: 18px; font-weight: 900; }"),
                        tags$style(type="text/css", ".jslider {color: #428bca; font-weight: bold;data-skin: 'blue';}"),



                        tabsetPanel(type = "tabs", position = "above",


                                    #### TAB: Model ####
                                    tabPanel(h4("Main"),

                                             #### subTabs ####
                                             tabsetPanel(type = "pills",
                                                         #### multiparameter plots ####
                                                         tabPanel("Visuals",

                                                                  tabsetPanel(

                                                                    tabPanel("Model parameters",
                                                                             wellPanel(
                                                                                       fluidRow(
                                                                                         uiOutput("ui_multiparam_sort"),
                                                                                         uiOutput("ui_multiparam_selectize"),
                                                                                         column(3, offset = 2, sliderInput("param_plot_ci_level", h5("Credible interval (%)"), min = 50, max = 95, value = 50, step = 5))
                                                                                       ),
                                                                                       bsCollapsePanel(title = "View options",  id="param_plot_customize", value="yes",
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
                                                                                                       ),
                                                                                                       hr(),
                                                                                                       uiOutput("buttons_param_plot")
                                                                                       )
                                                                             ),
                                                                             hr(),
                                                                             # plot
                                                                             plotOutput("plot_param_vertical_out")
                                                                    ),
                                                                    tabPanel("Markov chain autocorrelation",
                                                                             wellPanel(
                                                                                       uiOutput("ui_autocorr_customize")
                                                                             ),
                                                                             plotOutput("autocorr_plot_out"),
                                                                             hr(),
                                                                             fluidRow(
                                                                               column(3, offset = 9, bsButton("autocorr_isolation", label = "Isolation view")),
                                                                               bsTooltip(id = "autocorr_isolation", title = "Open plot in modal window", placement = "left", trigger = "hover")
                                                                             ),
                                                                             bsModal("autocorr_isolation_modal", "Markov chain autocorrelation", trigger = "autocorr_isolation",
                                                                                     tags$div(class = "span12",
                                                                                              plotOutput("autocorr_plot_out_isolation")
                                                                                     )
                                                                             )
                                                                    )
                                                                  )
                                                         ),

                                                         #### summary stats ####
                                                         tabPanel("Statistics",
                                                                  tabsetPanel(
                                                                    tabPanel("Posterior summary statistics",
                                                                             fluidRow(
                                                                               column(4, bsCollapsePanel(title = "View options",
                                                                                                         h5("Rounding"),
                                                                                                         fluidRow(
                                                                                                           column(3, numericInput("stats_digits", label = "", value = 1, min = 0, max = 7, step = 1)),
                                                                                                           column(8, offset = 1, h6("decimals"))
                                                                                                         ),
                                                                                                         hr(),
                                                                                                         checkboxGroupInput("stats_columns", label = h5("Columns"),
                                                                                                                            choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "Monte Carlo uncertainty (se_mean)" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                                                                                                            selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")),
#                                                                                                          bsButton("btn_open_glossary", "View glossary", style = "link"),
#                                                                                                          uiOutput("glossary_modal"),
                                                                                                         hr(),
                                                                                                         # export
                                                                                                         downloadButton("download_summary_stats", " Save table as .RData")
                                                                                )
                                                                               ),
                                                                               column(8, dataTableOutput("all_summary_out")
                                                                               )
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
                                             wellPanel(style = "border-color: #428bca; background-color: #FFFFFF; padding-top: 10px;",
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

                                                         #### trace plot #####
                                                         tabPanel("Trace",
                                                                  bsCollapsePanel(title = "View Options",  id="trace_options_collapse", value="trace_open",
                                                                                  fluidRow(
                                                                                    column(1, numericInput("trace_chain", label = "", min = 0, max = object@nChains, step = 1, value = 0)),
                                                                                    column(3, h6("Chain (0 = all chains)")),
                                                                                    column(4, offset = 1, bsToggleButton("trace_customize", label = h6("Customize appearance"), value = TRUE, style = "link", size = "mini")),
                                                                                    column(3, conditionalPanel(condition = "input.trace_customize == true", checkboxInput("user_trace_customize", h6("Load settings"), FALSE)))
                                                                                  ),
                                                                                  conditionalPanel(condition = "input.trace_customize == true",
                                                                                                   uiOutput("ui_trace_customize")
                                                                                  ),
                                                                                  hr(),
                                                                                  uiOutput("buttons_trace")
                                                                  ),
                                                                  br(),
                                                                  plotOutput("trace_plot_out")
                                                         ),

                                                         #### density plot #####
                                                         tabPanel("Density",
                                                                  bsCollapsePanel(title = "View Options",  id="density_options_collapse", value="density_open",
                                                                                  fluidRow(
                                                                                    column(1, numericInput("dens_chain", label = "", min = 0, max = object@nChains, step = 1, value = 0)),
                                                                                    column(3, h6("Chain (0 = all chains)")),
                                                                                    column(4, offset = 1,  bsToggleButton("dens_customize", label = h6("Customize appearance"), value = TRUE, style = "link", size = "mini")),
                                                                                    column(3, conditionalPanel(condition = "input.dens_customize == true", checkboxInput("user_dens_customize", h6("Load settings"), FALSE)))
                                                                                  ),
                                                                                  conditionalPanel(condition = "input.dens_customize == true",
                                                                                                             uiOutput("ui_density_customize")
                                                                                  ),
                                                                                  hr(),
                                                                                  uiOutput("buttons_density")
                                                                  ),
                                                                  plotOutput("density_plot_out")

                                                         ),

                                                         #### bivariate plot #####
                                                         tabPanel("Bivariate",
                                                                  # select 2nd parameter
                                                                  fluidRow(
                                                                    column(3, h5("Second parameter")),
                                                                    column(6, selectizeInput(inputId = "param2_contour", label = "", choices = rev(.make_param_list(object)), multiple = FALSE))
                                                                  ),
                                                                  bsCollapsePanel(title = "View Options",  id="contour_options_collapse", value="contour_open",
                                                                                  fluidRow(
                                                                                    column(1, h6("Style")),
                                                                                    #                                                                               uiOutput("ui_contour_customize_type"),
                                                                                    column(3, selectInput("contour_type", label = "", choices = c("Scatter", "Contour", "Point"), selected = "Scatter")),
                                                                                    column(4, offset = 1,  bsToggleButton("contour_customize", label = h6("Customize appearance"), value = TRUE, style = "link", size = "mini")),
                                                                                    column(3, conditionalPanel(condition = "input.contour_customize == true", checkboxInput("user_contour_customize", h6("Load settings"), FALSE)))
                                                                                  ),
                                                                                  conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Scatter')",
                                                                                                   uiOutput("ui_contour_customize_scatter")
                                                                                  ),
                                                                                  conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Contour')",
                                                                                                             uiOutput("ui_contour_customize_contour")
                                                                                  ),
                                                                                  conditionalPanel(condition = "(input.contour_customize == true && input.contour_type == 'Point')",
                                                                                                             uiOutput("ui_contour_customize_point")
                                                                                  ),
                                                                                  hr(),
                                                                                  uiOutput("buttons_contour")
                                                                  ),
                                                                  plotOutput("contour_plot_out")
                                                         )
                                             ) # END subTabs: parameter plots
                                    ), # END TAB: Individual Parameters


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
                                    tabPanel(h4("Notes"),
#                                              br(),
                                             helpText(strong("Use this space to store notes about your model")),
                                             p("The text will be saved in the",code("user_model_info"),
                                               "slot of your shinystan object and displayed here
                                               each time SHINYstan is launched for this object."),
                                             br(),
                                             tags$textarea(id="user_model_info", style = "width: 500px; border-color: #214565;", rows=20, cols=60, shiny_stan_object@user_model_info),
                                             br(),
                                             fluidRow(
                                               column(3, actionButton("save_user_model_info", label = "Save Changes")),
                                               column(8, offset = 1, textOutput("user_text_saved")),
                                               tags$style(type = "text/css", "#user_text_saved {color: gray;}")
                                             ),
                                             hr()
                                             #       h6("Why use this feature?"),
                                             #       helpText("If you want to allow other users to explore your model with",
                                             #                "SHINYstan, you can send them your shiny_stan_object and they",
                                             #                "will see any comments you've saved.")
                                    ), # END TAB: notes


                                    ## empty tabs
                                    tabPanel(""),tabPanel(""),
                                    #### TAB: About ####
#                                     tabPanel(h4(style = "color: #c4b0ac;", "About"),
                                    tabPanel(h4(style = "color: #c6dcef;", "About"),
#                                              h3("About SHINYstan"),
#                                              p("Coming soon."),
#                                              hr(),
                                             h3("Contributors"),
                                             h4("SHINYstan"),
                                             htmlOutput("ui_credits"),
                                             br(),
                                             h4("Stan & RStan"),
                                             a("Stan Development Team", href="http://mc-stan.org/team.html")
                                    ), # END TAB: About

                                    #### TAB: Help ####
#                                     tabPanel(h4(style = "color: #5e3c58;", "Help"),
                                    tabPanel(h4(style = "color: #214565;", "Help"),
                                             h3("SHINYstan help"),
                                             p("More coming soon."),
                                             bsButton("btn_help_appearance_settings", label = "Saving/loading appearance settings", style = "link"),
                                             uiOutput("help_modal_appearance_settings"),
                                             bsButton("btn_help_saving_ggplot", label = "Saving plots as ggplot2 objects", style = "link"),
                                             uiOutput("help_modal_saving_ggplot"),
                                             hr()
#                                              h4("Glossary"),
#                                              bsButton("btn_help_defn_rhat", label = "Rhat", style = "link"),
#                                              uiOutput("help_modal_defn_rhat"),
#                                              bsButton("btn_help_defn_n_eff", label = "n_eff", style = "link"),
#                                              uiOutput("help_modal_defn_n_eff"),
#                                              bsButton("btn_help_defn_se_mean", label = "se_mean", style = "link"),
#                                              uiOutput("help_modal_defn_se_mean")
                                    )


                        ) # END all tabs
              ) # END mainPanel
            ) # END verticalLayout
  ) # END fluidPage
) # END shinyUI
