output$glossary_modal_rhat <- renderUI({
  bsModal("glossary_rhat", "SHINYstan glossary", trigger = "btn_glossary_rhat",
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

output$glossary_modal_n_eff <- renderUI({
  bsModal("glossary_n_eff", "SHINYstan glossary", trigger = "btn_glossary_n_eff",
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

output$glossary_modal_se_mean <- renderUI({
  bsModal("glossary_se_mean", "SHINYstan glossary", trigger = "btn_glossary_se_mean",
          withMathJax(),
          p(h4("Monte Carlo uncertainty"), "\\(se_{\\rm mean}\\)"),
          p("The standard error of the mean of the posterior samples
            (not to be confused with the standard deviation of the posterior samples)
            is the uncertainty associated with the Monte Carlo approximation.
            This quantity approaches 0 as the number of samples goes to infinity,
            whereas the standard deviation of the posterior
            samples approaches the standard deviation of the posterior distribution.")
  )
})

output$glossary_modal <- renderUI({
  bsModal("glossary_all", "SHINYstan glossary", trigger = "btn_open_glossary",
          withMathJax(),
          h3("Glossary"),
          helpText("Coming soon.")
#           bsButton("btn_glossary_rhat", label = "Rhat", style = "link"),
#           uiOutput("glossary_modal_rhat"),
#           bsButton("btn_glossary_n_eff", label = "n_eff", style = "link"),
#           uiOutput("glossary_modal_n_eff"),
#           bsButton("btn_glossary_se_mean", label = "se_mean", style = "link"),
#           uiOutput("glossary_modal_se_mean")
  )
})

