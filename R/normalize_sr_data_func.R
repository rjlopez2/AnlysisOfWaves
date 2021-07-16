#' Normalize to control your values
#'
#' This function take a previously aggregated by cell dataset using the function `summarize_sr_by_cell_func()` end normalize all numeric variables to the control conditions. The new dataset contain a new variable called `Var_type` witch contain the `"Normalized"` value or the `"Raw"` value.
#' @param aggre_by_cell_dataset A Dataframe. A previously aggregated by cell dataset.
#'
#' @return A dataframe with nomrlaize numeric variables to control.
#' @export
#'
#' @examples # the example is missing
normalize_sr_data_func <- function(aggre_by_cell_dataset){

  aggre_by_cell_dataset <- aggre_by_cell_dataset %>%
    pivot_longer(cols = where(is.double),
                 names_to = "Parameters") %>%
    pivot_wider(names_from = .data$Condition,
                values_from = where(is.double)) %>%
    mutate(across(where(is.double) ,
                  list("Normalized" = ~. / Control))) %>%
    group_by(.data$Treatment, .data$Animal) %>%
    mutate(Control_Normalized =  .data$Control / mean(.data$Control, na.rm = T)) %>%
    pivot_longer(cols = where(is.double),
                 names_to = "Condition") %>%
    mutate(Var_type = if_else(str_detect(.data$Condition, "_Normalized$"),
                              "Normalized", "Raw"),
           Condition = forcats::as_factor(str_remove(.data$Condition, "_Normalized"))) %>%
    filter_all(all_vars(!is.na(.))) %>%
    pivot_wider(names_from = .data$Parameters,
                values_from = .data$value)

  return(aggre_by_cell_dataset)

}
