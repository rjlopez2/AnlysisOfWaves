
# load("~/Dropbox/r_stat/code/current_code/Intact_cell_waves/df40_o.RData")
# # df40_o <- df40_o %>%
df40_o %>%
#   # glimpse()# %>%
#   select(Date:Experiment, Condition, Animal, Animal_No,
#          starts_with("For"), starts_with("Wave"), starts_with("Caff"),
#          Groups, core_name )# %>%
  # names

  my_boxplot_and_jitter_func(yaxe = "Wave_speed_mean", faceted_by_1 = "Condition")

df40_o %>%
  # names
  clean_and_tidy_df_func(vars_for_analysis = wave_lat_and_freq) %>%
  # names
  # my_boxplot_and_jitter_func(yaxe = wave_kinet_parameters[[1]])
  multi_plot_list_func(my_plot_fun = my_boxplot_and_jitter_func,
                       yaxe_vars = wave_lat_and_freq)


wave_kinet_parameters[[1]]





wave_kinet_parameters <- df40_o %>%
# df40_o %>%
  select(starts_with("Wave_") / (contains("50") | contains("late") | contains("Freq"))) %>%
  clean_and_tidy_df_func()

    names
    select(Wave_deltaFF0_mean, Wave_tau_mean, Wave_speed_mean) %>%
    rlang::names2() %>%
    rlang::set_names()

  wave_kinet_parameters

  wave_lat_and_freq <- df40_o %>%
  # df40_o %>%
    select(starts_with("Wave_") / contains("5")) %>%
    select(Wave_latency, Wave_Freq_Hz) %>%
    rlang::names2() %>%  # taking wave kinetics parameters
    set_names() # converting to names

    wave_lat_and_freq
