#' Arrange multiples plots by condition
#'
#' This helper function make multiples plot base on the treatments provided, eg. Fab, cAMP and not by the parameter analyzed or your variable of interest.
#'
#' @param dataframe A dataframe object. The dataset for analysis, eg wave kinetics or wave occurrence df.
#' @param var Character vector. List of variable(s) you want to plot.
#' @param my_conditions Character vector. List of conditions you want to plot, eg. `c("Fab", "cAMP")`.
#' @param type_of_plot "Character. What kind of plot would you like to make? Options are 1. A regular boxplot using: `"reg_boxplot"`, 2. A superplot using: `"superplot"`. Default to `"superplot"`.
#' @param ... Additional parameters passed to the type of plot.
#'
#' @return a list of multiples ggplot objects.
#' @export
#'
#' @examples # the example is still missing.
plot_arrange_by_condition_func <- function(dataframe,
                                           var,
                                           my_conditions,
                                           type_of_plot = "superplot",
                                           ...){


  switch(type_of_plot,

         reg_boxplot = plot_lists <-purrr::map(my_conditions, function(.x){
             dataframe %>%
               filter(.data$Treatment == .x) %>%
               multi_plot_list_func(yaxe_vars = var,
                                    my_plot_fun = my_boxplot_and_jitter_func,
                                    ...)
           }) %>%
           purrr::flatten(),

         # superplot = plot_lists <- my_conditions %>%
         superplot = plot_lists <- purrr::map(my_conditions, function(.x){
           dataframe %>%
             filter(.data$Treatment == .x) %>%
             multi_plot_list_func(yaxe_vars = var,
                                  my_plot_fun = superplot_func,
                                  ...)
         }) %>%
           purrr::flatten(),

         stop("Invalid `type_of_plot` value. You must select an accepted plot type. Options are: 1. `reg_boxplot`, 2. `superplot`"))


  return(plot_lists)

}
