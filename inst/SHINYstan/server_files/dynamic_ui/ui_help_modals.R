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
          p("After customizing the appearance of a plot,",
            "click on the 'Save appearance settings' button",
            "to save the settings to a .RData file. ",
            "To load previously saved settings simply load",
            "the .RData file into your Global Environment",
            em("before"), "launching", strong("SHINYstan"),
            "and then click on 'Load settings' in the appropriate plot window.")
  )
})
