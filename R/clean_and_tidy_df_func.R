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
#'
clean_and_tidy_df_func <- function(my_df, vars_for_analysis){
  my_grouping_vars = c("filename", "Date", "Experiment", "Animal",
                       "Animal_No", "Condition", "Treatment", "n_paired")

  my_df <- my_df %>%
    add_count(.data$Date,
              .data$Experiment,
              name = "n_paired") %>%
    select(any_of(my_grouping_vars),
           all_of(vars_for_analysis)) %>%
    filter(!is.na(syms(paste0({{vars_for_analysis}}, collapse = " | ") )))# %>% # remove na values using symbols catch method
  # filter(n_paired == 2) %>%
  # rename_at(vars(ends_with("_mean")), str_sub, 1, -6)

  rm(my_grouping_vars)

  return(my_df)
}
