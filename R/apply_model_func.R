#' Apply a model-like function to dataset
#'
#' This function applies a model to a nested dataframe and return a new column with the model object. It accets function models expressions in the form of: `my_model <- funcion(my_dataset){ lm(value ~ condition_1 + condition_2, data = my_dataset) }`, where `value`, is the name of your variable of interest and `condition_1 + condition_2` are the explanatory variables. It can also use other type of model expression.
#' @param dataset A nested dataframe. Ideally, previously grouped by the paramters of interes to be analyzed. Eg. wave frequency, wave latency, amplitude, etc.
#' @param targed_dataset Character. the column name of the dataset to perform the summary. By default uses the colum named `"data"` containing the raw dataset, but if you have transformed data e.ge logarithmic, centered, etc., you can input the name of such column.
#' @param my_model A function. A function containing the model to apply to your targeted dataset.
#' @importFrom rlang :=
#' @return The input dataframe with a new column named `"my_model"` containing the model object.
#' @export
#'
#' @examples # the example is missing
apply_model_func <- function(dataset,
                             targed_dataset = "data",
                             my_model){
  targed_dataset <- sym(targed_dataset)

  dataset %>%
    mutate("{{my_model}}" := purrr::map(!!targed_dataset, my_model))# %>%

}
