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
