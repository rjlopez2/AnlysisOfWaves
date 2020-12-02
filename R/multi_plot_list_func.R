#' Multiples Plots
#'
#' Plots multiples variables (eg. scatter plots) from a dataframe and a plotting function.
#'
#' @param my_dataset A dataframe.
#' @param yaxe_vars Character vector of variables names to be plotted.
#' @param my_plot_fun Function. A function to be used for generating the plots
#' @param ... Other parameter to pass.
#'
#' @return A list of multiples ggplot objects.
#' @export
#'
#' @examples # The example is still missing...
#'
#'
multi_plot_list_func <- function(my_dataset, yaxe_vars, my_plot_fun, ...){

  map(yaxe_vars, function(.var){
    my_plot_fun(dataset = my_dataset,
                yaxe = .var,
                ...)
  })
}
