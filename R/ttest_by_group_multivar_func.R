#' Make T-test in grouping variables
#'
#' This function is similar to the function `wilcxntest_by_group_multivar_func` but using the parametric t-test for normally distributed datasets.
#'
#' @param my_dataset A cleaned dataframe.
#' @param my_var_set A character vector with the names of variables to test.
#' @param group_1 Character of one or more values. The name for the first grouping comparison variable. Default to `c("Treatment", "Condition")`.
#' @param group_2 Character. The name for the second grouping comparison variable. Default to `"Animal"`.
#' @param round_to Integer. A value to indicate number of decimals to used in final output. Default to 2.
#' @param p_adj_met Character. One of the following methods p adjust methods: `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"`. Defoult to `"BH`. for more info look at `p.adjust.methods`.
#' @param pairwise Logical. If TRUE (the default) the function `rstatix::pairwise_t_test()` is used, else it will use the `rstatix::t_test()` function. Read documentation for the differences.
#' @param ... Additional parameters passed to the function `rstatix::pairwise_t_test()`.
#'
#' @return a datatable with the output of the Wilcoxon test
#' @export
#'
#' @examples # no example made jet.
ttest_by_group_multivar_func <- function (my_dataset,
                                          my_var_set,
                                          group_1 = c("Treatment", "Condition"),
                                          group_2 = "Animal",
                                          round_to = 2,
                                          p_adj_met = "BH",
                                          pairwise = TRUE,
                                          ...){

  # group_1 <- syms(group_1)
  group_2 <- sym(group_2)
  result_table <- purrr::map_dfr(my_var_set, function(my_var) {
    my_var <- sym(my_var)

    my_dataset <- my_dataset %>%
      group_by(across(any_of(group_1))) #%>%

    if(pairwise == TRUE){

      my_dataset <- my_dataset %>%
        rstatix::pairwise_t_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),# need to fix this method for better aproach for handeling of formulas
                                 detailed = TRUE,
                                 paired = FALSE,
                                 p.adjust.method = p_adj_met,
                                 ...)

    }else{

      my_dataset <- my_dataset %>%
        rstatix::t_test(formula = stats::formula(expr(!!my_var ~ !!group_2)),
                        detailed = TRUE,
                        paired = FALSE,
                        p.adjust.method = p_adj_met,
                        ...)

    }
  })
  result_table <- result_table %>%
    dplyr::rename(Parameter = .data$.y.) %>%
    mutate(p.adjust.method = p_adj_met) %>%
    mutate(across(where(is.double),
                  ~round(.x, round_to)))
  return(result_table)
}
