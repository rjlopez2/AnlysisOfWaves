test_that("summary stats produce output in perme cells df", {

  summary_params <- c("n_Animals", "n", "mean", "sd", "sem", "median", "NormalityShapirop")
  summrized_df <- clean_SR_data %>% my_summ_stat_func()

  map(summary_params, ~ expect_output(str(summrized_df), .x))



})


test_that("summary stats produce output in Intact cells df", {

  summary_params <- c("n_Animals", "n", "mean", "sd", "sem", "median", "NormalityShapirop")
  summrized_df <- df_waves_intact_cells %>% my_summ_stat_func()

  map(summary_params, ~ expect_output(str(summrized_df), .x))



})
