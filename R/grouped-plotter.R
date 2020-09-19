grouped_plotter <- function(data,
                            x = NA,
                            y = NA,
                            fill = NA,
                            facet = NA,
                            scales = "free_y",
                            factorfill = FALSE) {
  facet <- ifelse(facet == "NA", NA, facet)

  fill <- ifelse(fill == "NA", NA, fill)

  y <- ifelse(y == "NA", NA, y)


  if (factorfill == TRUE & all(!is.na(fill))){

    if (!is.factor(data[[fill]])){

      data[[fill]] <- as.factor(data[[fill]])
    }


  }

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
        facet_wrap(vars(.data[[facet]]), scales = scales) +
        theme_minimal()

    } else {
      data %>%
        ggplot(aes(x = .data[[x]],
                   y = .data[[y]])) +
        geom_point(shape = 21, size = 4) +
        facet_wrap(vars(.data[[facet]]), scales = scales) +
        theme_minimal()

    }



  } else {

    if (is.numeric(data[[x]])){
      stat_name = "bin"
    } else {
      stat_name = "count"
    }

    if (!is.na(fill)) {
      data %>%
        ggplot(aes(x = .data[[x]], fill = .data[[fill]])) +
        geom_histogram(stat = stat_name) +
        facet_wrap(vars(.data[[facet]]), scales = scales) +
        theme_minimal()
    } else  {
      data %>%
        ggplot(aes(x = .data[[x]])) +
        geom_histogram(stat = stat_name) +
        facet_wrap(vars(.data[[facet]]), scales = scales) +
        theme_minimal()


    }
  }
}
