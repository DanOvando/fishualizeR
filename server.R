




server <- function(input, output, session) {
  # length composition ------------------------------------------------------
  grouped_plotter <- function(data,
                              x = NA,
                              y = NA,
                              fill = NA,
                              facet = NA,
                              scales = "free_y") {
    facet <- ifelse(facet == "NA", NA, facet)

    fill <- ifelse(fill == "NA", NA, fill)

    y <- ifelse(y == "NA", NA, y)



    if (is.na(facet)) {
      data$facet <-  ""
      facet <- "facet"
    }


    if (!is.na(x) & !is.na(y)) {
      if (!is.na(fill)) {
        data %>%
          ggplot(aes(
            x = .data[[x]],
            y = .data[[y]],
            fill = .data[[fill]]
          )) +
          geom_point(shape = 21, size = 4) +
          facet_wrap(vars(.data[[facet]]), scales = scales)

      } else {
        data %>%
          ggplot(aes(x = .data[[x]],
                     y = .data[[y]])) +
          geom_point(shape = 21, size = 4) +
          facet_wrap(vars(.data[[facet]]), scales = scales)

      }



    } else {
      if (!is.na(fill)) {
        data %>%
          ggplot(aes(x = .data[[x]], fill = .data[[fill]])) +
          geom_histogram() +
          facet_wrap(vars(.data[[facet]]), scales = scales)
      } else  {
        data %>%
          ggplot(aes(x = .data[[x]])) +
          geom_histogram() +
          facet_wrap(vars(.data[[facet]]), scales = scales)


      }
    }
  }

  lcomp_plotter <- function(data,
                            lbin = NA,
                            n = NA,
                            fill = NA,
                            facet = NA,
                            scales = "free_y") {
    facet <- ifelse(facet == "NA", NA, facet)

    fill <- ifelse(fill == "NA", NA, fill)

    if (is.na(facet)) {
      data$facet <-  ""
      facet <- "facet"
    }

    if (!is.na(fill)) {
      data %>%
        ggplot(aes(x = .data[[lbin]],
                   y = .data[[n]],
                   fill = .data[[fill]])) +
        geom_col(shape = 21, size = 4) +
        facet_wrap(vars(.data[[facet]]), scales = scales)

    } else {
      data %>%
        ggplot(aes(x = .data[[lbin]],
                   y = .data[[n]])) +
        geom_col(shape = 21, size = 4) +
        facet_wrap(vars(.data[[facet]]), scales = scales)

    }

  }


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

# filter and or subset data


# plot data

output$inspectplot <-
  renderPlot(inspect_plot(
    lcomps,
    x = input$inspectplot_x,
    y = input$inspectplot_y
  ))

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

counter <-
  function(data,
           group_var = NULL,
           length_col,
           n_col = NA,
           type = "obs") {
    group_var = ifelse(is.na(group_var), NULL, group_var)

    group_var <- c(group_var, length_col)

    if (type == "obs") {
      out <- data %>%
        group_by(across({
          {
            group_var
          }
        })) %>%
        count()
    } else if (type == "counts") {
      out <- data %>%
        dplyr::group_by(dplyr::across({
          {
            group_var
          }
        })) %>%
        dplyr::summarise(n = sum(.data[[n_col]]))

    }

    return(ungroup(out))


  }


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


  lcomp_plot <- eventReactive(input$plot_groupers,{

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
