pacman::p_load(tidyverse)
load_all()

document()
check()
install()

# load("~/Dropbox/r_stat/code/final_script/clean_SR_data.RData")
# clean_SR_data <- filtered_df
# use_data(clean_SR_data, overwrite = TRUE)
# use_data_raw()
# file.create("data-raw/playground_2.R")
use_r("clean_SR_data")

use_r("wilcxntest_by_group_multivar_func")
use_r("clean_and_tidy_df_func")
use_r("my_summ_stat_func")
use_r("sem_func")
use_r("df_waves_intact_cells")
use_r("my_boxplot_and_jitter_func")
use_r("my_cum_occu_wave_plot_func")
use_r("pptx_presentation_theme_func")
use_r("multi_plot_list_func")
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

