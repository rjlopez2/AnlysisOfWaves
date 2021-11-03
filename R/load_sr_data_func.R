#' load_sr_data_func
#'
#' @param my_dir String. the original path where the data is stored. this is only for SR experiments orpermeabilized cells?
#'
#' @return A datatframe with concatenated files of single experiments indicated in the searching path.
#' @export
#'
#' @examples # example is missing
load_sr_data_func <- function(my_dir){
  # file_list <- list.files(files_path, pattern  = "DataTable.xlsx", full.names = TRUE, recursive = TRUE)
  file_list <- list_dirs_depth_n_func(my_dir, 1) %>%
    list.files(., pattern = "DataTable\\.xlsx$", full.names = TRUE)
  #print(file_list)
  my_data <- purrr::map_dfr(file_list, function(file){
    print(c("reading file :", file))

    full_data <- readxl::read_xlsx(file, sheet = 4, col_names = TRUE, skip = 2)
  })

  my_data$Experiment <- str_sub(my_data$filename, start = 10, end = 15)
  remove(file_list)


  my_data <- my_data %>% add_Treatment_factor_func()

  my_data <- my_data %>%
    mutate(across(c(my_data$Experiment:my_data$Linescan, my_data$Animal_No), factor)) %>%
    mutate(Animal = factor(my_data$Animal, levels = c("CPVT-WT", "CPVT-HET"))) %>%
    mutate(Condition = factor(my_data$Condition,
                              levels = c("Control", "Fab", "cAMP", "Vehicle"))) %>%
    mutate(Treatment = factor(my_data$Treatment,
                              levels = c("cAMP", "Fab", "Vehicle")))

  return(my_data)

}
