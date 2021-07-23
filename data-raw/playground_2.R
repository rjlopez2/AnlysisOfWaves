
# ######### function to create random noise #########
# randon_generator <- function(x){
#   corrupt <- rbinom(length(x),1,0.90) # choose an average of 90% to corrupt at random
#   corrupt <- as.logical(corrupt)
#   noise <- rnorm(sum(corrupt),10,200) # generate the noise to add
#   x[corrupt] <- x[corrupt] + noise # replace and add noise
#   return(x)
# }


load("~/Dropbox/r_stat/code/current_code/Intact_cell_waves/df40_o.RData")

df_waves_intact_cells <- df40_o %>%
  # df40_o %>%
  # head
  #   # glimpse()# %>%
  select(Date:Experiment, Condition, Animal, Animal_No,
         starts_with("For"), starts_with("Wave"), starts_with("Caff"), starts_with("SR"),
         Groups, core_name ) %>%
  mutate(across(where(is.numeric), # transform the data and add random noise for security purpose
                randon_generator))
# head
# names
use_data(df_waves_intact_cells, overwrite = TRUE)
rename_files("df40_o", "df_waves_intact_cells")

my_boxplot_and_jitter_func(yaxe = "Wave_speed_mean", faceted_by_1 = "Condition")

df40_o %>%
  # names
  clean_and_tidy_df_func(vars_for_analysis = wave_lat_and_freq) %>%
  # names
  # my_boxplot_and_jitter_func(yaxe = wave_kinet_parameters[[1]])
  multi_plot_list_func(my_plot_fun = my_boxplot_and_jitter_func,
                       yaxe_vars = wave_lat_and_freq)


wave_kinet_parameters[[1]]
hbnbkjs

install_github("ropensci/gitignore")

wave_kinet_parameters <- df40_o %>%
  # df40_o %>%
  select(starts_with("Wave_") / (contains("50") | contains("late") | contains("Freq"))) %>%
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

df_waves_intact_cells %>%
  filter(`For Occu Analysis?` == "yes") %>%
  my_occu_bar_plot_func(reffer_wave_thres = 10, base_size = 18)

ggsave(paste(here::here(),
             paste(format(Sys.Date(),
                          "%y%m%d"),
                   "combined_occu_and_latency_plot.pdf",
                   sep = "_"),
             sep = "/"),
       device = "pdf",
       units = "cm",
       width = 44,
       height = 11)

load_all()
df_waves_intact_cells %>%
  # names
  multi_plot_list_func(yaxe_vars = "Wave_deltaFF0_mean",
                       my_plot_fun = superplot_func,
                       base_violin = "cells",
                       plot_type = "cells",
                       animal_layer = T,
                       wave_alpha = 0.4,
                       wave_size = 1,
                       cell_size = 2.5,
                       cell_alpha = 0.4,
                       animal_size = 4,
                       animal_alpha = 0.7,
                       base_font_size = 20)
head(c("Animal_No", "Animal", "Condition", "Treatment", "Experiment"), n = -1)
