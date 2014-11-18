output$help_modal_multiparam_sort <- renderUI({
  bsModal("help_multiparam_sort", "SHINYstan help", trigger = "btn_help_multiparam_sort",
          helpText("In the parameter selection dropdown menu",
                   "should parameter names x[j,k] be sorted by j or k?"),
          br(),
          br(),
          helpText(strong("Sorting by j: "), "x[1,1] x[1,2] x[2,1] x[2,2]"),
          br(),
          helpText(strong("Sorting by k: "), "x[1,1] x[2,1] x[1,2] x[2,2]")
  )
})


output$help_modal_appearance_settings <- renderUI({
  bsModal("help_appearance_settings", "SHINYstan help", trigger = "btn_help_appearance_settings",
          h4("Saving and loading appearance settings"),
          p("After customizing the appearance of a plot,
            click on the 'Save appearance settings' button
            to save the settings to a .RData file.
            To load previously saved settings simply load
            the .RData file into your Global Environment",
            em("before"), "launching", strong("SHINYstan"),
            "and then click on 'Load settings' in the
            appropriate plot window.")
  )
})

output$help_modal_saving_ggplot <- renderUI({
  bsModal("help_saving_ggplot", "SHINYstan help", trigger = "btn_help_saving_ggplot",
          h4("Saving plots as ggplot2 objects"),
          p("You can save any plots as ggplot2 objects by clicking on the
          'Save ggplot2 object' button. The object will be saved as an .RData
          file that you can load into your Global Environment using the",
          code("load"), "function in R. You can then make changes to
          the plot using the functions in the ggplot2 package."
          )
  )
})

output$help_modal_defn_rhat <- renderUI({
  bsModal("help_defn_rhat", "SHINYstan glossary", trigger = "btn_help_defn_rhat",
          withMathJax(),
          p(h4("Rhat"), "\\(\\hat{R}\\)"),
          p('The \\(\\hat{R} \\) statistic',
            "is an estimate of the factor by which the scale of the
            distribution of the posterior samples might be reduced
            in the limit as the number of samples goes to infinity.
            At convergence \\(\\hat{R} = 1\\) (although \\(\\hat{R} = 1\\)
            is not itself sufficient for convergence)."),
          p("Formally, let \\(n \\) be the number of iterations,
            and let \\(B \\) and \\(W \\) denote the between-chain
            and within-chain variances of the posterior simulations of the
            estimand of interest. Then \\(\\hat{R}\\) is computed as
            $$\\hat{R} = \\sqrt{\\frac{\\frac{n-1}{n}W + \\frac{1}{n}B}{W}}.$$"
            ),
          br(),
          p("From", em("Bayesian Data Analysis"), "(2nd ed.):",
            tags$blockquote(
              tags$small(
              "We recommend computing the potential scale reduction
              for all scalar estimands of interest; if \\(\\hat{R} \\)
              is not near 1 for all of them continue the simulastion runs [...]
              The condition of \\(\\hat{R} \\) being 'near' 1 depends on the
              problem at hand; for most examples, values below 1.1 are acceptable
              but for a final analysis in a critical problem, a higher level of
              precision may be required."
              )
            )
          )
  )
})

output$help_modal_defn_n_eff <- renderUI({
  bsModal("help_defn_n_eff", "SHINYstan glossary", trigger = "btn_help_defn_n_eff",
          withMathJax(),
          p(h4("Effective sample size"),"\\(n_{\\rm eff}\\)"),
          p("\\(n_{\\rm eff}\\) is an estimate of the effective
          sample size (or effective number of independent draws) from the
          posterior distribution of the estimand of interest. It is computed as
          $$n_{\\rm eff} = mn \\frac{\\frac{n-1}{n}W + \\frac{1}{n}B}{B},$$
          where \\(m\\) is the number of chains, \\(n\\) the number of samples
          per chain, and \\(B\\) and \\(W\\) are the between-chain and within-chain
          variances."),
          p("From", em("Bayesian Data Analysis"), "(2nd ed.):",
            tags$blockquote(tags$small(
              "If \\(m\\) is small, then \\(B\\) will have a high sampling variability,
              so that \\(n_{\\rm eff}\\) is a fairly crude estimate. We actually report
              \\(\\min{\\left(n_{\\rm eff}, mn\\right)}\\), to avoid claims that our simulation is more
              efficient than random sampling."
            )
            ))

  )
})

output$help_modal_defn_se_mean <- renderUI({
  bsModal("help_defn_se_mean", "SHINYstan glossary", trigger = "btn_help_defn_se_mean",
          p(h4("Monte Carlo uncertainty"), "\\(se_{\\rm mean}\\)"),
          p("The standard error of the mean of the posterior samples
            (not to be confused with the standard deviation of the posterior samples)
            is the uncertainty associated with the Monte Carlo approximation.
            This quantity approaches 0 as the number of samples goes to infinity,
            whereas the standard deviation of the posterior
            samples approaches the standard deviation of the posterior distribution.")
  )
})


