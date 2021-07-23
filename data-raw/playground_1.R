pacman::p_load(tidyverse)
load_all()

document()
check()
install()
# use_data_raw()
file.create("data-raw/playground_2.R")

use_r("wilcxntest_by_group_multivar_func")
use_r("clean_and_tidy_df_func")
use_r("my_summ_stat_func")
use_r("sem_func")
use_r("df_waves_intact_cells")
use_r("my_boxplot_and_jitter_func")
use_r("my_cum_occu_wave_plot_func")
use_r("pptx_presentation_theme_func")
use_r("multi_plot_list_func")
use_r("superplot_func")
use_r("my_occu_bar_plot_func")

use_r("cell_layer_vio_ggplot_func")
use_r("cell_layer_point_ggplot_func")
use_r("animal_layer_point_ggplot_func")
use_r("wave_layer_vio_ggplot_func")
use_r("wave_layer_point_ggplot_func")
use_r("superplot_func")

# functions mainly used in the permeabilized dataset
use_r("filter_SR_experiment_func")
use_r("summarize_sr_by_cell_func")
use_r("normalize_sr_data_func")
use_r("plot_arrange_by_condition_func")

# build_readme()

df40_o %>%
  # str
  # my_cum_occu_wave_plot_func(20)
  my_occu_bar_plot_func(10)
# mutate(new_waves = if_else(Wave_latency <= 10 &
#                              !is.na(Wave_latency) &
#                              Wave_latency > 0, T, F)) %>%
# group_by(.data$Animal, .data$Condition, .data$new_waves) %>%
# select(.data$Animal, .data$Condition, .data$new_waves) %>%
# summarise(count = n()) %>%
# mutate(Percentage = count/sum(count) * 100) %>%
# filter(.data$new_waves == T) #%>%
# ggplot2::ggplot(ggplot2::aes(x = .data$Animal,
#                              y = .data$Percentage,
#                              fill = .data$new_waves,
#                              alpha = 0.5)) +
# ggplot2::geom_bar(stat="identity",
#                   color = "black",
#                   ggplot2::aes(fill = .data$Animal),
#                   size = 1)
mutate(new_waves = if_else(Wave_latency <= 10 &
                             !is.na(Wave_latency) &
                             Wave_latency > 0, T, F)) %>%
  group_by(Animal, Condition, new_waves) %>%
  select(Animal, Condition, new_waves) %>%
  summarise(count = n()) %>%
  # View
  mutate(Percentage = count/sum(count) * 100) %>%
  filter(new_waves == T) %>%
  # View
  ggplot(aes(x = Animal,
             y = Percentage,
             fill = new_waves,
             alpha = 0.5))  +
  geom_bar(stat="identity",
           color = "black",
           aes(fill = Animal),
           size = 1) +
  ggplot2::facet_grid(. ~ .data$Condition)
head

"~/Dropbox/r_stat/Figures/Intact_cells/raw_linescan/latency_traces" %>%
  # paste("WT", "ISO", sep = "/") %>%
  # list.files(pattern = "*Smth\\.tif", full.names = T) %>%
  # make_profile_func()
  make_multi_profile_func() %>%
  # glimpse()
  str
?make_profile_func()
