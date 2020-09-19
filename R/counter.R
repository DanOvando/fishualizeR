counter <-
  function(data,
           group_var = NULL,
           length_col,
           n_col = NA,
           type = "obs",
           bin_width = NA) {
    group_var = ifelse(is.na(group_var), NULL, group_var)

    if (!is.na(bin_width)) {
      data$length_bin <-
        plyr::round_any(data[[length_col]], bin_width, f = floor) + bin_width / 2


    } else {

      data$length_bin <- data[[length_col]]

    }

    group_var <- c(group_var, "length_bin")

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
