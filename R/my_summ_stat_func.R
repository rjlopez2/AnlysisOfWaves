#' Compute summary statistics of multiples variables
#'
#' Compute the summary statistics of the multiples variables from the wave analysis from a previosly cleaned dataset
#'
#' @param my_dataset A dataframe prevously cleaned with the fucntion "lalala".
#'
#' @return compute the mean, median, SD, SEM, number of waves, number of cells, number of animals, and asses Normality of the distribution with the Shapiro-Wilk test.
#' @export
#'
#' @examples # The example is still missing...
my_summ_stat_func <- function(my_dataset){

  my_funs_names <- c("mean", "sd", "sem", "median", "n_Waves", "Normality_Shapiro_p")

  stats_by_cells <- my_dataset %>% group_by(Animal, Condition) %>%
    select(!(starts_with("SR")|starts_with("n_pa"))) %>%

    summarise_if(is.numeric, .funs = funs(n_Waves = length,
                                          mean = mean,
                                          sd = sd,
                                          sem = sem_func,
                                          median = median,
                                          Normality_Shapiro_p = shapiro.test(.)$p.value)) %>%

    pivot_longer(cols = contains(paste("_", my_funs_names, sep = "")),
                 names_to = "Parameters") %>%
    mutate(Stats = str_extract(Parameters, pattern = paste0(my_funs_names, collapse = "|")),
           Parameters = str_remove(Parameters, pattern = paste0(paste("_", my_funs_names, sep = ""), collapse = "|"))) %>%
    pivot_wider(names_from = Stats) %>%
    select(Parameters, Condition, everything()) %>%
    arrange(Parameters, Condition)

  cell_no <- my_dataset %>%
    distinct(Animal, Animal_No, Experiment, Condition, .keep_all = T) %>%
    group_by(Animal, Condition) %>%
    summarise(n_Cells = n())


  animal_no <- my_dataset %>%
    distinct(Animal, Animal_No) %>%
    group_by(Animal) %>%
    summarise(n_Animals = n())

  full_table <- full_join(stats_by_cells, cell_no) %>%
    full_join(y = animal_no)

  return(full_table)
  # return(stats_by_cells)
}
