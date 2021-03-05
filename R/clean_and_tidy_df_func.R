#' clean_and_tidy_df_func
#'
#' Clean and organize your dataset
#'
#' @param my_df A dataframe with raw data of waves. Must contain "Date" and "Experiment" columns.
#' @param vars_for_analysis A character vector with variable names to be analyzed.
#'
#' @return A cleaned dataframe ready for statistics analysis or plotting and add column "n_paired" for paired experiments.
#' @export
#'
#' @examples # The example is still missing...
clean_and_tidy_df_func <- function(my_df, vars_for_analysis){

  my_df <- my_df %>%
    add_count(.data$Date, .data$Experiment, name = "n_paired") %>%
    select(.data$filename, .data$Date, .data$Experiment, .data$Animal,
           .data$Animal_No, .data$Condition, .data$n_paired, {{vars_for_analysis}}) %>%
    # filter(!is.na(Wave_deltaFF0_mean | Wave_tau_mean | Wave_speed_mean)) %>% # remove na values in the conventional way
    filter(!is.na(syms(paste0({{vars_for_analysis}}, collapse = " | ") )))# %>% # remove na values using symbols catch method
    # filter(n_paired == 2) %>%
    # rename_at(vars(ends_with("_mean")), str_sub, 1, -6)

  return(my_df)

}
