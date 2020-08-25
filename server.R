



funs <- list.files(here::here("R"))

walk(funs, ~ source(here::here("R", .x)))

server <- function(input, output, session) {
  # length composition ------------------------------------------------------

  lcomps <- read_csv(here("data", "BiometricosTable.csv")) %>%
    janitor::clean_names()

  output$lcomps <-
    renderDataTable(lcomps,
                    options = list(pageLength = 5))

  output$inspectplot_x <- renderUI({
    vars <- colnames(lcomps[map_lgl(lcomps, is.numeric)])
    selectizeInput("inspectplot_x",
                   "Select X Variable from raw data to plot",
                   vars)
  })

  output$inspectplot_y <- renderUI({
    vars <- c(NA, colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput("inspectplot_y",
                   "Select Y Variable from raw data to plot",
                   vars)
  })



  # plot raw data data

  output$inspectplot <-
    renderPlot(inspect_plot(
      lcomps,
      x = input$inspectplot_x,
      y = input$inspectplot_y
    ))

  # assess data coverage

  output$cov_var_1 <- renderUI({

    vars <- colnames(lcomps)

    selectizeInput("cov_var_1",
                   "Select 1st variable to assess coverage",
                   vars)
  })

  output$cov_var_2 <- renderUI({

    vars <- c(NA,colnames(lcomps))

    selectizeInput("cov_var_2",
                   "Select 2nd variable to assess coverage",
                   vars)
  })

  output$data_tally <- renderDataTable(assess_coverage(lcomps,
                                                   group_var1 = input$cov_var_1,
                                                   group_var2 = input$cov_var_2,
                                                   length_col = input$select_ldata) %>%
                                         dplyr::mutate(pn = scales::percent(pn, accuracy = 0.01)) %>%
                                         dplyr::mutate(p_missing = scales::percent(p_missing, accuracy = 0.01)) %>%
                                         dplyr::rename("# of Observations" = n,
                                                       "% of Total Observations" = pn,
                                                       "# of Non-Missing Observations" = n_present,
                                                       "% of Observations Missing" = p_missing),
                                       options = list(pageLength = 5))
#
#   output$data_tally_plot <- renderPlot({
#
#     assess_coverage(lcomps,
#                     group_var1 = input$cov_var_1,
#                     group_var2 = input$cov_var_2,
#                     length_col = input$select_ldata) %>%
#       dplyr::mutate(pn = scales::percent(pn, accuracy = 0.01)) %>%
#       dplyr::mutate(p_missing = scales::percent(p_missing, accuracy = 0.01)) %>%
#       dplyr::rename("# of Observations" = n,
#                     "% of Total Observations" = pn,
#                     "# of Non-Missing Observations" = n_present,
#                     "% of Observations Missing" = p_missing) %>%
#       ggplot(aes())
#
#
#   })

  # if the thing is numeric, bin it. if it's discrete, convert things to "other" with less than a few observations.





  # aggregate length data

  output$select_ldata <- renderUI({
    vars <- c(NA, colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput("select_ldata",
                   "Select the column with length data in it",
                   vars)
  })

  output$select_tally <- renderUI({
    vars <- c(NA, colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput("select_tally",
                   "Select the column with the numbers per length bin in it",
                   vars)
  })

  output$select_groupers <- renderUI({
    vars <- c("Choose Grouping Variables" = "", colnames(lcomps))
    selectInput("select_groupers",
                "Select the variables to group by",
                vars,
                multiple = TRUE)
  })


  glcmps <- eventReactive(input$group, {
    grouped_lcomps <-
      counter(
        lcomps,
        group_var = input$select_groupers,
        length_col = input$select_ldata
      )
  })

  #https://mastering-shiny.org/action-transfer.html
  # remember tomorrow that you can return output of render / observed event as thing()
  output$grouped_lcomps <-
    renderDataTable(glcmps(),  options = list(pageLength = 5))

  output$download_grouper <- downloadHandler(
    filename = function() {
      "aggregated-lcomps.csv"
    },
    content = function(file) {
      vroom::vroom_write(glcmps(), file, ",")
    }
  )

  output$grouped_plot_x <- renderUI({
    vars <- c("Select one" = "", colnames(glcmps()))
    selectInput("grouped_x",
                "Choose what to plot on x-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_y <- renderUI({
    vars <- c(NA, colnames(glcmps()))
    selectInput("grouped_y",
                "Choose what to plot on y-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_fill <- renderUI({
    vars <- c(NA, colnames(glcmps()))
    selectInput("grouped_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_facet <- renderUI({
    vars <- c(NA, colnames(glcmps()))
    selectInput("grouped_facet",
                "Choose what to facet by",
                vars,
                multiple = FALSE)
  })

  group_plot <- eventReactive(input$plot_groupers, {
    grouped_plotter(
      glcmps(),
      x = input$grouped_x,
      y = input$grouped_y,
      fill = input$grouped_fill,
      facet = input$grouped_facet,
      scales = "free"
    )
  })

  output$group_plot <- renderPlot(group_plot())


  lcomp_plot <- eventReactive(input$plot_groupers, {
    lcomp_plotter(
      glcmps(),
      lbin = input$select_ldata,
      n = "n",
      fill = input$grouped_fill,
      facet = input$grouped_facet,
      scales = "free"
    )

  })

  output$lcomp_plot <- renderPlot(lcomp_plot())

}
