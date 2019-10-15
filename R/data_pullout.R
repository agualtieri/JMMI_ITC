### Pull out NGO specific data

data_pullout <- function (data, ngo_name) {
  
  if (ngo_name == "DRC" | ngo_name == "drc" | ngo_name == "IOM" | ngo_name == "iom") {
    
    ngo_name <- "drc"
    data_drc <- data %>% dplyr::select(starts_with(ngo_name))
    data_drc
    
  }
  
  else if (ngo_name == "NRC" | ngo_name == "nrc") {
    
    ngo_name <- tolower(ngo_name)
    data_nrc <- data %>% dplyr::select(starts_with(ngo_name))
    data_nrc
    
    
  }
  
  else if (ngo_name == "CARE" | ngo_name == "care") {
    
    ngo_name <- tolower(ngo_name)
    data_care <- data %>% dplyr::select(starts_with(ngo_name))
    data_care
    
  }
  
  else if (ngo_name == "SI" | ngo_name == "si") {
    
    ngo_name <- tolower(ngo_name)
    data_si <- data %>% dplyr::select(starts_with(ngo_name))
    data_si
    
  }
  
  else if (is.null(ngo_name)) {
    
    data_all <- data %>% dplyr::select(starts_with("all"))
    data_all
    
  }
}





library(xlsformfill)
library(tidyverse)
data <- xlsform_fill(questions = read.csv("./input/questions.csv"),
                     choices = read.csv("./input/choices.csv"),
                     5000)

data <- data[,-c(882:887)]


test_iom <- data_pullout(data, "iom")
test_drc <- data_pullout(data, "DRC")
test_care <- data_pullout(data, "CARE")
test_si <- data_pullout(data, "SI")
test_nrc <- data_pullout(data, "NRC")


write.csv(data, "./output/JMMI_mock_dataset.csv")
