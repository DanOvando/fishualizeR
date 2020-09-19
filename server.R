





funs <- list.files(here::here("R"))

walk(funs, ~ source(here::here("R", .x)))

server <- function(input, output, session) {
  # length composition ------------------------------------------------------



  lcomps <- read_csv(here("data", "BiometricosTable.csv")) %>%
    janitor::clean_names()

  ldata <- reactiveValues(lcomps = lcomps)

  output$max_length <- renderUI({
    sliderInput(
      "max_length",
      "Select Max. Realistic Length",
      min = 0,
      max = max(lcomps[, input$select_ldata], na.rm = TRUE),
      value =  max(lcomps[, input$select_ldata], na.rm = TRUE)
    )

  })

  output$max_length_factor <- renderUI({
    ldat <- lcomps[[input$select_ldata]]

    ratio_guess <-
      plyr::round_any(mean(ldat[ldat > input$max_length], na.rm = TRUE) / mean(ldat[ldat < input$max_length], na.rm = TRUE), 10)

    numericInput(
      "max_length_factor",
      "Enter Max Length Conversion",
      min = 0,
      max = NA,
      value =  ratio_guess
    )

  })

  output$min_length <- renderUI({
    sliderInput(
      "min_length",
      "Select Min. Realistic Length",
      min = 0,
      max = max(lcomps[, input$select_ldata], na.rm = TRUE),
      value = 0
    )
  })

  output$min_length_ratio <- renderUI({
    ldat <- lcomps[[input$select_ldata]]

    ratio_guess <-
      plyr::round_any(mean(ldat[ldat < input$min_length], na.rm = TRUE) / mean(ldat[ldat > input$min_length], na.rm = TRUE), 10)

    numericInput(
      "min_length_factor",
      "Enter Min Length Conversion",
      min = 0,
      max = NA,
      value =  ratio_guess
    )

  })


observeEvent(input$correct_units, {

    clcomps <- lcomps

    tmp <- clcomps[[input$select_ldata]]

    clcomps[tmp > input$max_length & !is.na(tmp) , input$select_ldata] <-
      clcomps[tmp > input$max_length & !is.na(tmp), input$select_ldata] / input$max_length_factor

    ldata$lcomps <- clcomps
  })

observeEvent(input$reset, {

  ldata$lcomps <- lcomps

})


  # output$cleaned_lcomps <-
  #   renderDataTable(clcomps(),  options = list(pageLength = 5))
  #
  #

 # observeEvent(input$correct_units,{lcomps <- clcomps()})


output$colnames <- renderUI({
  selectInput("colnames", "Select Columns To View", choices = colnames(lcomps), selected = colnames(lcomps)[1:pmin(10, ncol(lcomps))],multiple = TRUE)

})

  output$lcomps <-
    renderDataTable(ldata$lcomps[,input$colnames],
                    options = list(pageLength = 5))

  output$inspectplot_x <- renderUI({
    vars <- colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)])
    selectInput("inspectplot_x",
                   "Select X Variable from raw data to plot",
                   vars,
                   selected = input$select_ldata)
  })

  output$inspectplot_y <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)]))
    selectInput("inspectplot_y",
                   "Select Y Variable from raw data to plot",
                   vars)
  })



  # plot raw data data

  output$inspectplot <-
    renderPlot(inspect_plot(
      ldata$lcomps,
      x = input$inspectplot_x,
      y = input$inspectplot_y
    ))

  # assess data coverage

  output$cov_var_1 <- renderUI({
    vars <- colnames( ldata$lcomps)

    selectizeInput("cov_var_1",
                   "Select 1st variable to assess coverage",
                   vars)
  })

  output$cov_var_2 <- renderUI({
    vars <- c(NA, colnames( ldata$lcomps))

    selectizeInput("cov_var_2",
                   "Select 2nd variable to assess coverage",
                   vars)
  })


  output$cov_var_3 <- renderUI({
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata
    ) %>%
      dplyr::rename(
        "# of Length Observations" = n,
        "% of Total Length Observations" = pn,
        "# of Non-Missing Length Observations" = n_present,
        "% of Length Observations Missing" = p_missing
      )


    vars <-
      c(colnames(tmp)[(which(colnames(tmp) == "# of Length Observations"):ncol(tmp))])

    selectInput("cov_var_3",
                "Select coverage metric to plot",
                vars,
                selected = "# of Length Observations")
  })


  output$data_tally_plot <- renderPlot({
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata
    ) %>%
      dplyr::rename(
        "# of Length Observations" = n,
        "% of Total Length Observations" = pn,
        "# of Non-Missing Length Observations" = n_present,
        "% of Length Observations Missing" = p_missing
      )



    axis_vars <-
      colnames(tmp)[1:(which(colnames(tmp) == "# of Length Observations") - 1)]

    if (length(axis_vars) == 1) {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[input$cov_var_3]])) +
        geom_col()

    } else {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[axis_vars[2]]], fill = .data[[input$cov_var_3]])) +
        geom_tile() +
        scale_fill_viridis_c()


    }


    outplot

  })


  output$data_tally <- renderDataTable(
    assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata
    ) %>%
      dplyr::mutate(pn = scales::percent(pn / 100, accuracy = 0.01)) %>%
      dplyr::mutate(p_missing = scales::percent(p_missing / 100, accuracy = 0.01)) %>%
      dplyr::rename(
        "# of Observations" = n,
        "% of Total Observations" = pn,
        "# of Non-Missing Observations" = n_present,
        "% of Observations Missing" = p_missing
      ),
    options = list(pageLength = 5)
  )
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

    vars <- colnames(lcomps[map_lgl(lcomps, is.numeric)])

    guess <- vars[stringr::str_detect(vars,"(length)|(longitud)")][1]

    vars <- c(NA,vars)

    selectizeInput("select_ldata",
                   "Select the column with length data in it",
                   vars,
                   selected = guess)
  })

  output$select_tally <- renderUI({
    vars <- c(NA, colnames( ldata$lcomps[map_lgl( ldata$lcomps, is.numeric)]))
    selectizeInput("select_tally",
                   "Select the column with the numbers per length bin in it",
                   vars)
  })

  output$select_groupers <- renderUI({
    vars <- c("Choose Grouping Variables" = "", colnames( ldata$lcomps))
    selectInput("select_groupers",
                "Select the variables to group by",
                vars,
                multiple = TRUE)
  })


  glcmps <- eventReactive(input$group, {
    grouped_lcomps <-
      counter(
        ldata$lcomps,
        group_var = input$select_groupers,
        length_col = input$select_ldata,
        bin_width = input$bin_width
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
      factorfill = input$factorfill,
      scales = "free"
    )
  })

  output$group_plot <- renderPlot(group_plot())


  lcomp_plot <- eventReactive(input$plot_groupers, {
    lcomp_plotter(
      glcmps(),
      lbin = "length_bin",
      n = "n",
      fill = input$grouped_fill,
      facet = input$grouped_facet,
      factorfill = input$factorfill,
      scales = "free"
    )

  })

  output$lcomp_plot <- renderPlot(lcomp_plot())

}
