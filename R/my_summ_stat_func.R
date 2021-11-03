utils::globalVariables("where")
#' Summary statistics of multiples variables
#'
#'Compute the summary statistics of numeric variables from a given dataset. Compute the `mean`, `sd`, `sem`, `median`, `n_Waves`, `n_Cells`, `n_Animals` and `Normality_Shapiro_p.` NOTE: Shapiro-Wilk test will fail if less than 2 samples!.
#'
#' @param my_dataset A previously cleaned dataframe with the function `clean_and_tidy_df_func()`.
#' @param my_grouping_vars Character vector. A character vector of groups names assigned to perform the aggregation. Don't change at least you know what you are doing!
#' @param Na_rm Boolean. Shall Nas be removed for summary functions computation?. Default to TRUE.
#' @param round_to Integer. A value to indicate number of decimals to used in final output. Default to 2.
#' @param shapiro Character. asses shapiro wilk test for normality. Requiere dataset length bigger than 2, otherwise will raise a warning.
#'
#' @return Return a dataframe with the mean, median, SD, SEM, number of elements analized (cells or wave), number of animals, and asses Normality of the distribution with the Shapiro-Wilk test.
#' @export
#'
#' @examples # the example is missing
my_summ_stat_func <- function(my_dataset,
                              my_grouping_vars = c("Animal", "Treatment", "Condition"),
                              Na_rm = TRUE,
                              round_to = 2,
                              shapiro = "yes"){

  ###### check for n datapoints <= 2 to allow Shapiro test of normality    ######

  test_minimal_n <- my_dataset %>%
    group_by(across(any_of(my_grouping_vars))) %>%
    summarise(across(where(is.double),
                     ~ n() <= 3),
              .groups = "drop") %>%
    filter(across(where(is.logical), ~ .x == T))

  if(dim(test_minimal_n)[1] != 0){
    message("You have at least one variable with 2 or less datapoints.\nShapiro test for normality check requires n == 3 or more. Shapiro test will not be assesed!")
    shapiro = "no"

  }

  # create summary df

  switch(shapiro,
         yes = summarized_DF <- my_dataset %>%
           group_by(across(any_of(my_grouping_vars))) %>%
           summarise(across(where(is.double),
                            list(n = ~ length(.x),
                                 mean = ~ mean(.x, na.rm = Na_rm),
                                 sd = ~ stats::sd(.x, na.rm = Na_rm),
                                 sem = ~ sem_func(.x, na.rm = Na_rm),
                                 median = ~ stats::median(.x, na.rm = Na_rm),
                                 NormalityShapirop = ~ stats::shapiro.test(.x)$p.value),
                            .names = "{.col}_{.fn}"), .groups = "drop"),
         no = summarized_DF <- my_dataset %>%
           group_by(across(any_of(my_grouping_vars))) %>%
           summarise(across(where(is.double),
                            list(n = ~ length(.x),
                                 mean = ~ mean(.x, na.rm = Na_rm),
                                 sd = ~ stats::sd(.x, na.rm = Na_rm),
                                 sem = ~ sem_func(.x, na.rm = Na_rm),
                                 median = ~ stats::median(.x, na.rm = Na_rm)),
                            # NormalityShapirop = ~ stats::shapiro.test(.x)$p.value),
                            .names = "{.col}_{.fn}"), .groups = "drop")
  )

  # make it tidy
  rearranged_table <- summarized_DF %>%
    pivot_longer(cols = contains("_"),
                 names_to = "Parameters") %>%
    mutate(stats = str_extract(.data$Parameters,
                               pattern = "([^_]*)$"),

           Parameters = str_extract(.data$Parameters,
                                    pattern = "([^_]*_[^_]*)")) %>%
    pivot_wider(names_from = .data$stats) %>%
    mutate(across(where(is.double), ~ round(.x, round_to)))

  # calculate separately animal no
  animal_no <- my_dataset %>%
    group_by(across(any_of(my_grouping_vars))) %>%
    distinct(across(any_of(c("Animal", "Condition", "Animal_No")))) %>%
    summarise(n_Animals = n(), .groups = "drop")

  if( "Treatment" %in% colnames(rearranged_table)){
    final_table <- left_join(rearranged_table,
                                  animal_no,
                                  by = my_grouping_vars) %>%
      relocate(.data$n_Animals,
               .before = .data$n)
  }

  if( !"Treatment" %in% colnames(rearranged_table)){
    final_table <- left_join(rearranged_table,
                                  animal_no,
                                  by = my_grouping_vars[-2]) %>%
      relocate(.data$n_Animals,
               .before = .data$n)
  }


  rm(summarized_DF, animal_no, rearranged_table)

  return(final_table)
}
