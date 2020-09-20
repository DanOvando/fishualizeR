





funs <- list.files(here::here("R"))

walk(funs, ~ source(here::here("R", .x)))

server <- function(input, output, session) {

  # length composition ------------------------------------------------------

  ldata <- reactiveValues(lcomps = NA)

  tmp <- reactive({


    if (input$example == FALSE){
      req(input$file)
    }
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv file")
    )

    tmp <- read.csv(input$file$datapath) %>%
      janitor::clean_names()

    # ldata$lcomps <- tmp

    # ldata$oglcomps <- tmp

  })

  observeEvent(input$file, {

    ldata$lcomps <- tmp()

    ldata$oglcomps <- tmp()


  })

  tmp2 <- reactive({

    lcomps <- read.csv(here::here("data","BiometricosTable.csv")) %>%
      janitor::clean_names()

  })

  observeEvent(input$example,{

    if (input$example == TRUE){

    ldata$lcomps <- tmp2()

    ldata$oglcomps <- tmp2()
    }

  })



  output$max_length <- renderUI({

    if (input$example == FALSE){
    req(input$file)
    }
    sliderInput(
      "max_length",
      "Select Max. Realistic Length",
      min = 0,
      max = max(ldata$lcomps[, input$select_ldata], na.rm = TRUE),
      value =  max(ldata$lcomps[, input$select_ldata], na.rm = TRUE)
    )

  })

  output$max_length_factor <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    ldat <- ldata$lcomps[[input$select_ldata]]

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
      max = max(ldata$lcomps[, input$select_ldata], na.rm = TRUE),
      value = 0
    )
  })

  output$min_length_ratio <- renderUI({
    ldat <- ldata$lcomps[[input$select_ldata]]

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

    clcomps <- ldata$lcomps

    tmp <- clcomps[[input$select_ldata]]

    clcomps[tmp > input$max_length & !is.na(tmp) , input$select_ldata] <-
      clcomps[tmp > input$max_length & !is.na(tmp), input$select_ldata] / input$max_length_factor

    ldata$lcomps <- clcomps

    updateTabsetPanel(session, inputId="inspect", selected = "Plot Raw Data")

  })

observeEvent(input$reset, {

  ldata$lcomps <- ldata$oglcomps

})


  # output$cleaned_lcomps <-
  #   renderDataTable(clcomps(),  options = list(pageLength = 5))
  #
  #

 # observeEvent(input$correct_units,{lcomps <- clcomps()})


output$colnames <- renderUI({

  if (input$example == FALSE){
    req(input$file)
  }
  selectInput("colnames", "Select Columns To View", choices = colnames(ldata$lcomps), selected = colnames(ldata$lcomps)[1:pmin(10, ncol(ldata$lcomps))],multiple = TRUE)

})

  output$lcomps <-
    DT::renderDataTable({

      if (input$example == FALSE){
        req(input$file)
      }
      ldata$lcomps[,input$colnames]},
                    filter = "top",
                    options = list(pageLength = 5, autoWidth = TRUE))


  # inspect raw data

  output$raw_plot_x <- renderUI({
    vars <- c("Select one" = "", colnames(ldata$lcomps))
    selectInput("raw_x",
                "Choose what to plot on x-axis",
                vars,
                multiple = FALSE,
                selected = input$select_ldata)
  })

  output$raw_plot_y <- renderUI({
    vars <- c(NA,  colnames(ldata$lcomps))
    selectInput("raw_y",
                "Choose what to plot on y-axis",
                vars,
                multiple = FALSE)
  })

  output$raw_plot_fill <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps))
    selectInput("raw_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })

  output$raw_plot_facet <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps))
    selectInput("raw_facet",
                "Choose what to facet by",
                vars,
                multiple = FALSE)
  })

  raw_plot <- eventReactive(input$plot_raw, {
    grouped_plotter(
      ldata$lcomps,
      x = input$raw_x,
      y = input$raw_y,
      fill = input$raw_fill,
      facet = input$raw_facet,
      factorfill = input$raw_factorfill,
      scales = "free"
    )
  })

  output$raw_plot <- renderPlot(raw_plot())



  #  output$inspectplot_x <- renderUI({
  #   vars <- colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)])
  #   selectInput("inspectplot_x",
  #                  "Select X Variable from raw data to plot",
  #                  vars,
  #                  selected = input$select_ldata)
  # })
  #
  # output$inspectplot_y <- renderUI({
  #   vars <- c(NA, colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)]))
  #   selectInput("inspectplot_y",
  #                  "Select Y Variable from raw data to plot",
  #                  vars)
  # })



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

    if (input$example == FALSE){
      req(input$file)
    }
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata,
      n_col = input$select_tally
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

    if (input$example == FALSE){
      req(input$file)
    }
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata,
      n_col = input$select_tally
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

  cov_data <-   reactive({

    if (input$example == FALSE){
      req(input$file)
    }
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
    )})

  output$data_tally <- DT::renderDataTable(
    {

      if (input$example == FALSE){
        req(input$file)
      }
      cov_data()
      },
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE)
  )

  output$download_coverage <- downloadHandler(
    filename = function() {
      "data-coverage.csv"
    },
    content = function(file) {
      vroom::vroom_write(cov_data(), file, ",")
    }
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

    vars <- colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)])

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
    vars <- c("Choose Grouping Variables" = "", colnames(ldata$lcomps)[!colnames(ldata$lcomps) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
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
        bin_width = input$bin_width,
        n_col = input$select_tally
      )
  })

  #https://mastering-shiny.org/action-transfer.html
  # remember tomorrow that you can return output of render / observed event as thing()
  output$grouped_lcomps <-
    DT::renderDataTable(glcmps(), filter = "top", options = list(pageLength = 5, autoWidth = TRUE))

  output$download_grouper <- downloadHandler(
    filename = function() {
      "aggregated-lcomps.csv"
    },
    content = function(file) {
      vroom::vroom_write(glcmps(), file, ",")
    }
  )

  output$grouped_plot_x <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c("Select one" = "", colnames(glcmps()))
    selectInput("grouped_x",
                "Choose what to plot on x-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_y <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
    selectInput("grouped_y",
                "Choose what to plot on y-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_fill <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
    selectInput("grouped_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_facet <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])

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
