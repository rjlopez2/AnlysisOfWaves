#' Title
#'
#' @param files_path String. the original path where the data is stored. this is only for SR experiments orpermeabilized cells?
#'
#' @return A datatframe with concatenated files of single experiments indicated in the searching path.
#' @export
#'
#' @examples # Example is missing
load_sr_data <- function(files_path){

  # file_list <- list.files(files_path, pattern  = "DataTable.xlsx", full.names = TRUE, recursive = TRUE)
  file_list <- list.dirs.depth.n_func(files_path, 1) %>%
    list.files(., pattern = "DataTable\\.xlsx$", full.names = TRUE)
  #print(file_list)
  my_data <- lapply(file_list, function(file){
    print(file)

    full_data <- readxl::read_xlsx(file, sheet = 4, col_names = TRUE, skip = 2)
  })

  my_data <-  do.call("rbind.data.frame", my_data)
  # my_data$Groups <- as.factor(paste(my_data$Condition, my_data$Animal))
  # my_data$paredID <- str_sub(my_data$filename, end = 22)
  my_data$Experiment <- as.factor(str_sub(my_data$filename, start = 10, end = 15))
  my_data$Inc_time_min <- as.factor(my_data$Inc_time_min)
  my_data$Date <- as.factor(my_data$Date) # this has to be added in orderr to avoid the error with  faceting by date in ggplot or error when filtering
  my_data$Animal <- factor(my_data$Animal, levels = c("BL6", "CPVT-WT", "CPVT-HET"))
  my_data$Animal_No <- as.factor(my_data$Animal_No)
  my_data$Wave_No <- as.factor(my_data$Wave_No)
  my_data$Linescan <- as.factor(my_data$Linescan)
  my_data$SR_Baseline <- my_data$SR_Baseline * -1
  # my_data$Condition <- factor(my_data$Condition, levels = c("Control", "Vehicle",
  #                                                           "cAMP", "Fab",
  #                                                           "CDN11_10uM", "Fab-cAMP"))
  my_data$For_Analysis <- T
  #my_data$
  my_data <- my_data %>%
    add_Treatment_factor_func()
  my_data <- as.data.frame(my_data)
  my_data$Condition <- factor(my_data$Condition, levels = c("Control", "Vehicle",
                                                            "cAMP", "Fab",
                                                            "CDN11_10uM", "Fab-cAMP"))
  my_data$Treatment <- factor(my_data$Treatment, levels = c("Vehicle", "cAMP", "Fab"))

  remove(file_list)
  return(my_data)

}

