#' Make summary statistics on nested dataset
#'
#' This function compute descriptive statistics on a nested dataframe and create 3 new columns containing: 1) descriptive statistics using the function `my_summ_stat_func()`, 2) outliers and extreme values and 3) qqplot to visulaize normality.
#'
#' @param nested_df A nested dataset. Ideally, previously grouped by the paramters of interes to be analyzed. Eg. wave frequency, wave latency, amplitude, etc.
#' @param faceted_by_1 A string. Grouping variables for visualization. Default to `Condition`.
#' @param faceted_by_2 A string. Aditional grouping variables for visualization. Default to `.`.
#' @param targed_dataset Character. the column name of the dataset to perform the summary. By default uses the colum named `"data"` containing the raw dataset, but if you have transformed data e.ge logarithmic, centered, etc., you can input the name of such column.
#'
#' @return A nested dataframe with the additional columns: "Described", "outliers" and "qqplots" for the given input dataframe.
#' @export
#'
#' @examples # the example is missing
describe_nested_df <- function(nested_df,
                               targed_dataset = "data",
                               faceted_by_1 = "Condition",
                               faceted_by_2 = "."){

  targed_dataset <- sym(targed_dataset)
   my_parameter_name <- sym(names(nested_df)[1])

  df <- nested_df %>%

    # describe the basics statistics

    mutate(described = purrr::map(!!targed_dataset, ~(.x %>%
                                                        my_summ_stat_func(round_to = 2) %>%
                                                        pivot_longer(where(is.double)) %>%
                                                        drop_na(value) %>%
                                                        mutate(Parameters = str_sub(.data$Parameters,
                                                                                             start = 1, end = 5)) %>%
                                                        pivot_wider(values_from = value,
                                                                    names_from = name)))) %>%
    # check for autliers and extreme values

    mutate(outliers = purrr::map(!!targed_dataset, ~ (.x %>%
                                                 group_by(across(any_of(c("Animal",
                                                                          "Treatment",
                                                                          "Condition")))) %>%
                                                 rstatix::identify_outliers(value)))) %>%
    #make qq-plots

    # mutate(qq_plot = map(data, ~(.x %>%
    mutate(qq_plot = purrr::map(!!targed_dataset, ~(.x %>%
                                                      ggplot2::ggplot(ggplot2::aes(sample = value,
                                                                                   color = Animal)) +
                                                      qqplotr::stat_qq_band(ggplot2::aes(fill = Animal), alpha = 0.1) +
                                                      qqplotr::stat_qq_line() +
                                                      qqplotr::stat_qq_point() +
                                                      ggplot2::ggtitle(label = paste0("Parameter = ", !!my_parameter_name)) +
                                                      # ggplot2::facet_grid(~Condition, scales = "free_x") +
                                                      ggplot2::facet_grid(stats::reformulate(faceted_by_1, faceted_by_2), scales = "free_x") +
                                                      ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")))) #%>%
    # Check for homogeneity of variance via levene's test and report p value

    # apply_model_func(my_model = levn_test) %>%
    # mutate(levn_pval =  purrr::map_dbl(levn_test, ~.[["Pr(>F)"]][[1]]))
    # mutate(levn_pval =  purrr::map_dbl(.data$levn_test, ~.[["Pr(>F)"]][[1]]))


  return(df)

}
