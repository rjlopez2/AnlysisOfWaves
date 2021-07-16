#' Normalize to control your values
#'
#' This function take a previously aggregated dataset using the function `summarize_sr_by_cell_func()` end normalize all numeric variables to the control conditions. hte new dataset contain a variable call `Var_type` witch contain the `Normalized` value or the `Raw` value.
#' @param aggre_by_cell_dataset A Dataframe. A previously aggregated by cell dataset.
#'
#' @return A dataframe with nomrlaize numeric variables to control.
#' @export
#'
#' @examples # the example is missing
normalize_sr_data_func <- function(aggre_by_cell_dataset){

  filtered_dataset <- filtered_dataset %>%
    pivot_longer(cols = where(is.double),
                 names_to = "Parameters") %>%
    pivot_wider(names_from = .data$Condition,
                values_from = where(is.double)) %>%
    mutate(across(where(is.double),
                  list("Normalized" = ~. / Control))) %>%
    mutate(across(.data$Control_Normalized,
                  ~ .data$Control / mean(.data$Control))) %>%
    pivot_longer(cols = where(is.double),
                 names_to = "Condition") %>%
    mutate(Var_type = if_else(str_detect(.data$Condition, "_Normalized$"),
                              "Normalized", "Raw"),
           Condition = forcats::as_factor(str_remove(.data$Condition, "_Normalized"))) %>%
    filter_all(all_vars(!is.na(.))) %>%
    pivot_wider(names_from = .data$Parameters,
                values_from = .data$value) %>%
    ungroup

  return(filtered_dataset)

}
