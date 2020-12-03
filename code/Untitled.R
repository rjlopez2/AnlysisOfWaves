# df40_o <- df40_o %>%
df40_o %>%
  glimpse()# %>%
  select(Date:Experiment, Condition, Animal,
         starts_with("For"), starts_with("Wave"), starts_with("Caff"),
         Groups, core_name ) %>%
  names
  # names
  my_boxplot_and_jitter_func(yaxe = "Wave_speed_mean", faceted_by_1 = "Condition")

df40_o %>%
  filter(`For_Wave_Kinetics_Analysis?` == "yes") %>%
  filter(Wave_latency <= 10) %>%
  multi_plot_list_func(my_plot_fun = my_boxplot_and_jitter_func,
                       yaxe_vars = c("Wave_latency",
                                     "Wave_Freq_Hz"))
