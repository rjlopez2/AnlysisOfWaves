#' Compute summary statistics of multiples variables
#'
#' Compute the summary statistics of the multiples variables from the wave analysis of a previously cleaned dataset. Compute the `mean`, `sd`, `sem`, `median`, `n_Waves`, `n_Cells`, `n_Animals` and `Normality_Shapiro_p.` NOTE: Shapiro-Wilk test will fail if less than 2 samples!.
#'
#' @param my_dataset A previously cleaned dataframe with the function `clean_and_tidy_df_func()`.
#' @param vars_prefix_to_summ a string indicating which variables you want to summarize. Only one of this two values possible: "Cyto" or "SR".
#' @param Na_rm = Boolean. Shall Nas be removed for summary functions computation?. Default to TRUE.
#' @name my_summ_stat_func
#' @return return the mean, median, SD, SEM, number of waves, number of cells, number of animals, and asses Normality of the distribution with the Shapiro-Wilk test.
#' @export
#' @examples # The example is still missing...
#'
utils::globalVariables("where")
my_summ_stat_func <- function(my_dataset,
                                   vars_prefix_to_summ = c("Cyto", "SR"),
                                   Na_rm = TRUE){

  my_funs_names <- c("mean", "sd", "sem", "median", "n_Waves", "Normality_Shapiro_p")

  if (vars_prefix_to_summ == "Cyto"){

    vars_prefix_to_summ <- "SR"

  }else if(vars_prefix_to_summ == "SR"){

    vars_prefix_to_summ <- "Cyto"

  }else(stop("You must select 'Cyto' or 'SR' for the variables to be summarized"))

  # compute stats
  stats_by_cells <- my_dataset %>%
    group_by(across(c(.data$Animal,
                      any_of(.data$Treatment),
                      .data$Condition))) %>%
    select(!(starts_with(vars_prefix_to_summ)|starts_with("n_pa"))) %>%
    summarise_if(is.numeric,
                 .funs = list(n_Waves = ~ length(.x),
                              mean = ~ mean(.x, na.rm = Na_rm),
                              sd = ~ stats::sd(.x, na.rm = Na_rm),
                              sem = ~ sem_func(.x, na.rm = Na_rm),
                              median = ~ stats::median(.x, na.rm = Na_rm),
                              Normality_Shapiro_p = ~ stats::shapiro.test(.x)$p.value))
  # re-arrange table
  rearranged_table <- stats_by_cells %>%
    pivot_longer(cols = ends_with(paste("_",
                                        my_funs_names,
                                        sep = "")),
                 names_to = "Parameters") %>%
    mutate(stats = str_extract(.data$Parameters,
                               pattern = paste0(my_funs_names, collapse = "|")),
           Parameters = str_extract(.data$Parameters,
                                    pattern = "([^_]*_[^_]*)")) %>%
    pivot_wider(names_from = .data$stats) %>%
    mutate(across(where(is.double), ~ round(.x, 2))) # round to 2 decimals

  # compute cell number and waves number
  cell_no <- my_dataset %>%
    group_by(across(c(.data$Animal,
                      .data$Condition,
                      any_of(.data$Treatment)))) %>%
    distinct(.data$Animal,
             .data$Condition,
             .data$Animal_No,
             .data$Experiment) %>%
    summarise(n_Cells = n())


  animal_no <- my_dataset %>%
    group_by(.data$Animal,
             .data$Condition) %>%
    distinct(.data$Animal,
             .data$Condition,
             .data$Animal_No) %>%
    summarise(n_Animals = n())

  # final join of stats and n(s)

  full_table <- full_join(rearranged_table, cell_no) %>%
    full_join(y = animal_no) %>%
    relocate(.data$Normality_Shapiro_p, .before = .data$n_Waves) #%>% # reorder columns


  return(full_table)
  # return(stats_by_cells)
  # return(rearranged_table)
}
