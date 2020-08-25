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
