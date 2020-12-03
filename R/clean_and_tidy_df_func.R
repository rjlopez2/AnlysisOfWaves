clean_and_tidy_df_func <- function(my_df,
                                   vars_for_analysis = wave_kinet_parameters){

  my_df <- my_df %>%
    mutate(Experiment = str_sub(string = filename, 10, 15),
           Date_plus_experiment = str_sub(string = filename, 1, 15)) %>%
    add_count(Date_plus_experiment, name = "n_paired") %>%
    select(filename, Date, Experiment, Animal, Animal_No, Condition, n_paired, vars_for_analysis) %>%
    # names
    # filter(!is.na(Wave_deltaFF0_mean | Wave_tau_mean | Wave_speed_mean)) %>% # remove na values in the conventional way
    filter(!is.na(syms(paste0(vars_for_analysis, collapse = " | ") ))) %>% # remove na values using symbols catch method
    # filter(n_paired == 2) %>%
    rename_at(vars(ends_with("_mean")), str_sub, 1, -6)

  return(my_df)

}
