lcomp_plotter <- function(data,
                          lbin = NA,
                          n = NA,
                          fill = NA,
                          facet = NA,
                          scales = "free_y",
                          factorfill = FALSE) {
  facet <- ifelse(facet == "NA", NA, facet)

  fill <- ifelse(fill == "NA", NA, fill)

  if (factorfill == TRUE & all(!is.na(fill))){

    if (!is.factor(data[[fill]])){

      data[[fill]] <- as.factor(data[[fill]])
    }


  }

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
      facet_wrap(vars(.data[[facet]]), scales = scales) +
      theme_minimal()

  } else {
    data %>%
      ggplot(aes(x = .data[[lbin]],
                 y = .data[[n]])) +
      geom_col(shape = 21, size = 4) +
      facet_wrap(vars(.data[[facet]]), scales = scales) +
      theme_minimal()


  }

}
