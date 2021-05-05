#' Compute summary statistics of multiples variables
#'
#' Compute the summary statistics of the multiples variables from the wave analysis from a previosly cleaned dataset. Compute the `mean`, `sd`, `sem`, `median`, `n_Waves`, `n_Cells`, `n_Animals` and `Normality_Shapiro_p.` NOTE: Shapiro-Wilk test will fail if less than 2 samples!.
#'
#' @param my_dataset A dataframe prevously cleaned with the function `clean_and_tidy_df_func()`.
#' @param vars_prefix_to_summ a string indicating which variables you want to summarize. Only one of twon values possible: "Cyto" or "SR".
#'
#' @return compute the mean, median, SD, SEM, number of waves, number of cells, number of animals, and asses Normality of the distribution with the Shapiro-Wilk test.
#' @export
#'
#' @examples # The example is still missing...
my_summ_stat_func <- function(my_dataset,
                              vars_prefix_to_summ = c("Cyto", "SR")){

  if (vars_prefix_to_summ == "Cyto"){

    vars_prefix_to_summ <- "SR"

  }else if(vars_prefix_to_summ == "SR"){

    vars_prefix_to_summ <- "Cyto"

  }else(stop("You must select 'Cyto' or 'SR' for the variables to be summarized"))



  my_funs_names <- c("mean", "sd", "sem", "median", "n_Waves", "Normality_Shapiro_p")

  stats_by_cells <- my_dataset %>% group_by(across(c(.data$Animal,
                                                     any_of(.data$Treatment),
                                                     .data$Condition))) %>%
    select(!(starts_with(vars_prefix_to_summ)|starts_with("n_pa"))) %>%
    #
    summarise_if(is.numeric, .funs = list(n_Waves = ~ length(.x),
                                          mean = ~ mean(.x),
                                          sd = ~ stats::sd(.x),
                                          sem = ~ sem_func(.x),
                                          median = ~ stats::median(.x),
                                          Normality_Shapiro_p = ~ stats::shapiro.test(.x)$p.value)) %>%

    pivot_longer(cols = contains(paste("_",
                                       my_funs_names,
                                       sep = "")),
                 names_to = "Parameters") %>%
    mutate(Stats = str_extract(.data$Parameters,
                               pattern = paste0(my_funs_names,
                                                collapse = "|")),
           Parameters = str_remove(.data$Parameters,
                                   pattern = paste0(paste("_",
                                                          my_funs_names,
                                                          sep = ""),
                                                    collapse = "|"))) %>%
    pivot_wider(names_from = .data$Stats) %>%
    select(.data$Parameters,
           any_of(.data$Treatment),
           .data$Condition,
           everything()) %>%
    arrange(across(c(.data$Parameters,
                     any_of(.data$Treatment),
                     .data$Condition)))

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

  full_table <- full_join(stats_by_cells, cell_no) %>%
    full_join(y = animal_no)

  return(full_table)
  # return(stats_by_cells)
}
