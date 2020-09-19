assess_coverage <-
  function(data,
           group_var1 = NULL,
           group_var2 = NULL,
           length_col = NA,
           n_col = NA,
           type = "obs") {

    if (group_var2 == "NA"){
      group_var2 <- NULL
    }

    if (length_col != "NA"){
    group_vars <- c(group_var1, group_var2)

    obs <-  nrow(data)


    out <- data %>%
      dplyr::group_by(dplyr::across({
        {
          group_vars
        }
      })) %>%
      dplyr::summarise(n = length(.data[[length_col]]),
                       n_present =  sum(!is.na(.data[[length_col]])),
                       p_missing = mean(is.na(.data[[length_col]])) * 100) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pn = (n / obs) * 100)
    } else {
      out <- NA
    }

    return(out)


  }
