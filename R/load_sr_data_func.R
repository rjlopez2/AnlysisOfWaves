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
    cat(paste("reading file ---->>>>", file, sep = "\n"))

    full_data <- readxl::read_xlsx(file, sheet = 4, col_names = TRUE, skip = 2)
  })

# create TExperiment variable
  my_data$Experiment <- str_sub(my_data$filename, start = 10, end = 15)


# create Treatment factor
  my_data <- my_data %>% add_Treatment_factor_func()

# create set to factor other variables
  my_data <- my_data %>%
    mutate(across(c(.data$Experiment:.data$Animal_No), factor)) %>%
    mutate(Animal = forcats::fct_relevel(.data$Animal,
                                     c("CPVT-WT", "CPVT-HET"),
                                     after = 0)) %>%
    mutate(Condition = forcats::fct_relevel(.data$Condition,
                              c("Control", "Fab", "cAMP", "Vehicle"),
                              after = 0)) %>%
    mutate(Treatment = forcats::fct_relevel(.data$Treatment,
                              c("cAMP", "Fab", "Vehicle"),
                              after = 0)) %>%
    mutate(Date = as.character(.data$Date))

  remove(file_list)

  return(my_data)

}
