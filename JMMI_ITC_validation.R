# clear workspace
rm(list = ls())

install.packages(c("rlang", "devtools","questionr" ))

# check if package is installed
.is.package.installed<-function(package.name){
  package.name %in% installed.packages()[,"Package"]
}
#  install dependencies if missing
.install_dependencies<-function(packages.to.install){
  new.packages <- packages.to.install[!.is.package.installed(packages.to.install)]
  if(length(new.packages)) install.packages(new.packages)
  return(packages.to.install)
}

# install reachR

.install_reachR<-function(reinstall_if_exists = F, branch="master"){
  if(!.is.package.installed("reachR") | reinstall_if_exists){
    # get devtools if needed
    if(!.is.package.installed("devtools")){install.packages("devtools")}
    require("devtools")
    install_github("mabafaba/reachR2",ref = branch)
    # unload devtools
    detach("package:devtools", unload=TRUE)
    if(reinstall_if_exists){
      warning("Please restart R session to update reachR documentation")
    }
  }
}

.install_reachR(T)


# you might need some of these
library(usethis)
library(devtools)
require("dplyr")
require("knitr")
require("data.table")
require("questionr") 
require("reachR")
require("koboloadeR")
require("rlang")


# set wd to this script's folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data:
#data<-load_data(file = "./data/october2018.csv")

library(readr)

#download koboloadeR
library(koboloadeR)

# Months -> update according to the month being analyzed
current_month <- "august_2019"

# Load cleaned dataset from kobo server
datasets <- kobo_datasets(user = "reach_yemen:KOBOyemREACH2017", api = "kobohr") # copy the formid of the dataset you want to process

# download the dataset you want to process
data.frame <- kobo_data_downloader(formid = "396800", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")
data.frame

# check number of submission
kobo_submission_count("396800", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")

#if a cleaned version is need
library(readxl)
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("C:/Users/ACTED/Documents/R/Projects/yemen_jmmi")
data.frame<-read_csv("Inputs/august_2019_clean.csv")

#removing excess labels from column names, couldnt figure out how to remove from the actual download so did it this way
colnames(data.frame)<-sub('market_info/','',colnames(data.frame))
colnames(data.frame)<-sub('exchange_rate_market/','',colnames(data.frame))
colnames(data.frame)<-sub('water_trucking_rates_by_distance/','',colnames(data.frame))
colnames(data.frame)<-sub('water_trucking_service_information_001/','',colnames(data.frame))
colnames(data.frame)<-sub('water_trucking_service_information/','',colnames(data.frame))
colnames(data.frame)<-sub('WASH/','',colnames(data.frame))
colnames(data.frame)<-sub('Fuel/','',colnames(data.frame))
colnames(data.frame)<-sub('Info/','',colnames(data.frame))
colnames(data.frame)<-sub('Petrol/','',colnames(data.frame))
colnames(data.frame)<-sub('Petrol_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('Diesel/','',colnames(data.frame))
colnames(data.frame)<-sub('Diesel_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('fuel_supply_information/','',colnames(data.frame))
colnames(data.frame)<-sub('bottled_water/','',colnames(data.frame))
colnames(data.frame)<-sub('bottled_water_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('treated_water/','',colnames(data.frame))
colnames(data.frame)<-sub('treated_water_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('soap/','',colnames(data.frame))
colnames(data.frame)<-sub('soap_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('laundry_powder/','',colnames(data.frame))
colnames(data.frame)<-sub('laundry_powder_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('sanitary_napkins_powder/','',colnames(data.frame))
colnames(data.frame)<-sub('sanitary_napkins_price_information/','',colnames(data.frame))
colnames(data.frame)<-sub('wash_supply_information/','',colnames(data.frame))
colnames(data.frame)<-sub('General_Comments/','',colnames(data.frame))

data <- data.frame

# standard cleaning checks:
issues<-rbind(
  data %>% data_validation_all_others,
  data %>% data_validation_find_duplicates("_uuid"), # usually check in UUID
  data %>% data_validation_outliers
)

#issues %>% write.csv("./Output/potential_issues_",current_month,"REACH.csv", row.names = FALSE)


# percent by governorate:

gov_percent<-data %>% aggregate_percent(split.by = "governorate.name.en",
  write.to.file = "./output/gov_percent.csv",
  ignore.missing.data = F)


# count by governorate to check sample size:

gov_count<-data %>% aggregate_percent(split.by = "governorate.name.en",
  write.to.file = "./output/gov_count.csv",
  ignore.missing.data = F)




# what's up with different supplier types in a single row?
# vendor_type_combos<-data[,grep("type.supplier",names(data))] %>% lapply(table) %>% melt  %>% head(20)
#  they don't seem to  match, will ask what's up



# any sensitive information
data %>% names %>% grep("enum|gps|phone|num|name",.,value = T) %>% paste0(collapse="\n") %>% cat


#additinoal issues data required
#look up the org, can indicate if it was fixed or now, and the uuid to find in the quicksheets
#add the fields nessecary for the appended issues table, these will be populated using the for loop
issues$org <-0
issues$fixed<-0
issues$quants_issues<-0
issues$prices_issues<-0
issues$calc_issues<-0
issues$uuid<-0

#for loop along the entire sequence of entire issues based on the index
for (i in seq_along(issues$index)){
  #is the issue type is not ;other' response. may need recoding continue.
  if (issues$issue_type[i] != "'other' response. may need recoding."){
    #get the row number for which the observation occurs within the data frame
    rownum <- issues$index[i]
    #append the organization which has the faulty issue into the issues table
    issues$org[i] <- data[rownum,"select_one_organisation_name"]
    #find the colnum of the varialbe which produced teh issue using the match function with the name
    colnum<-match(issues$variable[i],names(data))
    #extract the name of the column, to be used in later if statements
    colnamed<-issues$variable[i]
    #attach the uuid to observation for easy look up later
    issues$uuid[i]<-data[get("rownum"), "_uuid"]
    #create call back numbers based on the positions of the variable you want to look at, these will be used to pull data from positions
    colnum_quant<-colnum-2
    colnum_price<-colnum-1
    colnum_calc<-colnum+1
    #only pull if the colnamed begins with calc_ (^indicates to grepl that it begins)
    if (grepl("^calc_",colnamed )){
      #will pull the quantity and prices into the issues part of the text to compare
      issues$quants_issues[i]<- data[[get("rownum"), get("colnum_quant")]]
      issues$prices_issues[i]<- data[[get("rownum"), get("colnum_price")]]
    }else if(grepl("^price_",colnamed)){
      #if begins with price it will pull the calc
      issues$calc_issues[i]<-data[get("rownum"), get("colnum_calc")]
    }
    
  }
}

library(readr) 
library(readxl)
issues2<-apply(issues,2,as.character)
write.csv(issues2, file = paste0("Outputs/potential_issues_",current_month,"_REACH.csv"), row.names = FALSE)

#so need to add in something for price and calc
#using fixed example column just put in value and uuid to select which thing has been fixed or checked

#issues$fixed[issues$value =="1" & issues$uuid == "87035ca8-20d7-44b6-ad0b-f9f16cbbe39a"] <- 1

# x<-data.frame%>%
#+ filter(`Info/select_one_organisation_name`=="NFDHR")
# table(x$`Info/district_ID`)

