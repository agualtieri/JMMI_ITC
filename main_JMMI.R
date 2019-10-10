## Cash and Market Portfolio - JMMI ITC
## Yemen Data Unit
## V1
## 10/10/2019

rm(list=ls())



## Main Yemen Market Monitoring Script ##
# clear R environment
rm(list=ls())

# set wd to this script's locations
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

# Load dependencies
source("./Scripts/basic scripts/load_dependencies.R")

# Load data preparation scripts
source("./Scripts/basic scripts/add_pcodes.R")
source('./Scripts/basic scripts/add_locations.R')
source("./Scripts/basic scripts/moveme.R")
source("./Scripts/basic scripts/delete_observations.R")

# Load calculators
source("./Scripts/basic scripts/calculate_smeb.R")
source("./Scripts/basic scripts/calculate_medians.R")
source("./Scripts/basic scripts/multiple_response.R")

library(openxlsx)
# Months -> update according to the month being analyzed
current_month <- "august_2019"

#create new workbook


# Load cleaned dataset from kobo server
#datasets <- kobo_datasets(user = "reach_yemen:KOBOyemREACH2017", api = "kobohr") # copy the formid of the dataset you want to process

# download the dataset you want to process
#data.frame <- kobo_data_downloader(formid = "360811", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")
#data.frame

# check number of submission
#kobo_submission_count("360811", user = "reach_yemen:KOBOyemREACH2017", api = "kobohr")

august_2019_jmmi_clean <- read_csv("Inputs/august_2019_clean.csv")
data.frame<-august_2019_jmmi_clean


## Delete the empty columns of price display ##
data.frame <- dplyr::select(data.frame, -c("start", "end", "today", "deviceid", "enumerator_id", "select_one_organisation_name", "org_name_other", "wash_list", "price_cubic_meter", "note_exchange_rate", contains("display"), contains("normalised"), "__version__", "_id", "_submission_time", "_validation_status", "_index"))

# Add Pcodes or Locatin names to data.frame and move them at the beginning of the data.frame
data.frame.named <- add.location(data.frame)
data.frame.named<- data.frame.named[moveme(names(data.frame.named), "country_name after date_survey; country_ID after country_name; governorate_name after country_ID; governorate_ID after governorate_name; district_name after governorate_ID; district_ID after district_name")]


## Delete districts that have less than 3 observations (see. methodology)
data.frame.validated <- delete.districts(data.frame.named, "district_ID", 3)

## Save final dataset ###
write.csv(data.frame.validated, file = paste0("./Outputs/final_validated_",current_month,".csv"), row.names = FALSE)
write.csv(data.frame.validated, file = paste0("./Inputs/final_validated_",current_month,".csv"), row.names = FALSE)

# Medians for dataset
data.medians.district <- calculate.medians(data.frame.validated, "district_ID")
write.csv(data.medians.district, file = paste0("Outputs/data_district_medians_",current_month,".csv"), row.names = FALSE)

data.medians.governorate <- calculate.medians(data.medians.district, "governorate_ID")
write.csv(data.medians.governorate , file = paste0("Outputs/data_governorate_medians_",current_month,".csv"), row.names = FALSE)

# SMEB for dataset
data.smeb.district <- calculate.smeb(data.medians.district, "district_ID")
write.csv(data.smeb.district, file = paste0("Outputs/data_district_SMEB_",current_month,".csv"), row.names = FALSE)

data.smeb.governorate <- calculate.smeb(data.medians.governorate, "governorate_ID")
write.csv(data.smeb.governorate, file = paste0("Outputs/data_governorate_SMEB_",current_month,".csv"), row.names = FALSE)


###################
## Data Analysis ##
###################

## Select only consecutive months
# Import CVS file ##
# Load previous and current month from cleaned datasets -> needs to be updated
previous.month <- read_csv("Outputs/final_validated_july_2019.csv")
current.month <- read_csv("Outputs/final_validated_august_2019.csv")

# Select unique ID from current month to match on previous month
uniqueID <- unique(previous.month$district_ID)

current.month.analysis <- current.month %>% subset(district_ID %in% uniqueID)

# Save matched file
write.csv(current.month.analysis, file = paste0("./Outputs/analysis_",current_month,".csv"), row.names = FALSE)
write.csv(current.month.analysis, file = paste0("./Inputs/analysis_",current_month,".csv"), row.names = FALSE)

# Medians calculations #
# Calculate district, governorate, national medians using the matched file
district_medians <- calculate.medians(current.month.analysis, "district_ID")
write.csv(district_medians, file = paste0("Outputs/district_medians_",current_month,"w_last_month.csv"), row.names = FALSE)

governorate_medians <- calculate.medians(district_medians, "governorate_ID")
write.csv(governorate_medians, file = paste("Outputs/governorate_medians_",current_month,"w_last_month.csv"), row.names = FALSE)

country_medians <- calculate.medians(governorate_medians, "country_ID")
write.csv(country_medians, file = paste("Outputs/country_medians_",current_month,"w_last_month.csv"), row.names = FALSE)

# SMEB calculations #
# Calculate district, governorate, national WASH SMEB using the matched file
district_smeb <- calculate.smeb(current.month.analysis, "district_ID")
write.csv(district_smeb, file = paste0("Outputs/district_smeb_",current_month,"w_last_month.csv"), row.names = FALSE)

governorate_smeb <- calculate.smeb(district_smeb, "governorate_ID")
write.csv(governorate_smeb, file = paste0("Outputs/governorate_smeb_",current_month,"w_last_month.csv"), row.names = FALSE)

country_smeb <- calculate.smeb(governorate_smeb, "country_ID")
write.csv(country_smeb, file = paste0("Outputs/country_smeb_",current_month,"w_last_month.csv"), row.names = FALSE)


# Calculate percentage change (I still need to create a function for this)
# Import CVS file - the file needs to be created manually by merging the governorate level medians ##
ts <- read_csv("Inputs/timeseries/JMMI_timeseries_R_v6.csv")
#ts$date <- as.Date(ts$date, format="%d/%m/%Y")
ts$date <- lubridate::dmy(ts$date)

## Calculate month to month percentage change ##
pct <- function(x) {((x/lag(x))-1)*100}


# District level #
district_ts <- ts %>%
  group_by(district_ID) %>%
  arrange(date) %>%
  mutate_at(vars(calc_price_petrol, calc_price_diesel,	calc_price_bottled_water,	calc_price_treated_water,	calc_price_soap,	calc_price_laundry,	calc_price_sanitary,	cost_cubic_meter,	exchange_rate), pct)


write.csv(district_ts, file = paste0("Outputs/ts_district_",current_month,".csv"), row.names = FALSE)


# Governorate level #
governorate_ts <- ts %>% 
  group_by(governorate_ID) %>% 
  arrange(date) %>%
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)

write.csv(governorate_ts, file = paste0("Outputs/ts_governorate_",current_month,".csv"), row.names = FALSE)


## Calculate national percentage change ##
national_ts <- read_csv("Inputs/timeseries/JMMI_national_timeseries.csv")

national_ts <- national_ts %>% 
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)

write.csv(national_ts, file = paste0("Outputs/ts_country_",current_month,".csv"), row.names = FALSE)

################################################################################
## Other variables (not sure if it makes sense to create functions for these) ##
################################################################################

## Calculating avg restocking times per governorate 
## Pivoting to select the columns of interest, and pivoting by grouping them by name and calculating the avg ##
df.restock.avg <- current.month.analysis %>%
  dplyr::select(governorate_name, contains("restock")) %>%
  group_by(governorate_name) %>%
  summarise_all(funs(mean), na.rm = TRUE)

write.csv(df.restock.avg, file = paste0("Outputs/restock_times_",current_month,".csv"), row.names = FALSE)

## Proportion challenges per vendor 
mean_challenges_fuel <- multiple.response(current.month.analysis, c("fuel_constraints_multiple/"))
write.csv(mean_challenges_fuel[["fuel_constraints_multiple/"]], file = paste0("Outputs/fuel_challenges_",current_month,".csv"), row.names = TRUE)

mean_challenges_wash <- multiple.response(current.month.analysis, c("wash_constraints_multiple/"))
write.csv(mean_challenges_wash[["wash_constraints_multiple/"]], file = paste0("Outputs/wash_challenges_",current_month,".csv"), row.names = TRUE)

mean_challenges_wt <- multiple.response(current.month.analysis, c("constraints_multiple/"))
write.csv(mean_challenges_wt[["constraints_multiple/"]], file = paste0("Outputs/WT_challenges_",current_month,".csv"), row.names = TRUE)

## Calculation of CASH proportions
df.cash <- current.month.analysis %>%
  dplyr::select(contains("cash_feasibility"))

mean.cash <- colMeans(df.cash == TRUE, na.rm = TRUE)

write.csv(mean.cash, file = paste0("Outputs/cash_",current_month,".csv"), row.names = TRUE)

## Water trucking analysis
## Summary statistics for water trucking (median truck capacity, median distance from location, median additional costs)
df.water.trucking.median <- current.month.analysis %>%
  dplyr::select(capacity_truck,	location_source,	additional_cost_5,	additional_cost_10) %>%
  summarise_all(funs(median), na.rm = TRUE)

write.csv(df.water.trucking.median, file = paste0("Outputs/water_trucking_",current_month,".csv"), row.names = FALSE)

## Proportion of water source mentioned ##
df.water.source <- current.month.analysis %>%
  dplyr::select(type_water) %>%
  filter(!is.na(type_water)) %>%
  group_by(type_water) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.water.source, file = paste0("Outputs/water_source_",current_month,".csv"), row.names = FALSE)

## Proportion of vendors applying an increase due to distance
df.delivery.costs <- current.month.analysis %>%
  dplyr::select(distance_price) %>%
  filter(!is.na(distance_price)) %>%
  group_by(distance_price) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.delivery.costs, file = paste0("Outputs/delivery_cost_",current_month,".csv"), row.names = FALSE)

## Proportion of chlorinated water
df.chlorinated.water <- current.month.analysis %>%
  dplyr::select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated)) %>%
  group_by(water_chlorinated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.chlorinated.water, file = paste0("Outputs/chlorination_",current_month,".csv"), row.names = FALSE)

## Proportion of type of owner ##
df.owner <- current.month.analysis %>% 
  dplyr::select(type_owner) %>%
  filter(!is.na(type_owner)) %>%
  group_by(type_owner) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

write.csv(df.owner, file = paste0("Outputs/type_owner_",current_month,".csv"), row.names = FALSE)


## Issues affecting supply chain ##
## % of Vendor KIs reporting issues affecting supply chain
supply.yesno <- current.month.analysis %>% 
  dplyr::select(mrk_supply_routes) %>%
  filter(!is.na(mrk_supply_routes)) %>%
  group_by(mrk_supply_routes) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

write.csv(supply.yesno, file = paste0("Outputs/market_supply_yesno_",current_month,".csv"), row.names = FALSE)

## Multiple response - issues affecting supply chain
issues.supply <- multiple.response(current.month.analysis, "mrk_supply_issues/")

write.csv(issues.supply[["mrk_supply_issues/"]], file = paste0("Outputs/market_supply_issues_",current_month,".csv"), row.names = FALSE)


## Infrastrcutural damage ##
infra.damage <- multiple.response(current.month.analysis, "mrk_dmg_infra/")

write.csv(infra.damage[["mrk_dmg_infra/"]], file = paste0("Outputs/infra_damage_",current_month,".csv"), row.names = TRUE)

## Supply increase: FUEL ##
supply.increase.fuel50 <- current.month.analysis %>%
  dplyr::select(mrk_increse_fuel_50) %>%
  filter(!is.na(mrk_increse_fuel_50)) %>%
  group_by(mrk_increse_fuel_50) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.fuel100 <- current.month.analysis %>%
  dplyr::select(mrk_increse_fuel_100) %>%
  filter(!is.na(mrk_increse_fuel_100)) %>%
  group_by(mrk_increse_fuel_100) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.fuel <- bind_rows(supply.increase.fuel50, supply.increase.fuel100)

write.csv(supply.increase.fuel, file = paste0("Outputs/supply_increase_fuel_",current_month,".csv"), row.names = FALSE)


## Supply increase: SMEB ##
supply.increase.WASH50 <- current.month.analysis %>%
  dplyr::select(mrk_increse_WASH_50) %>%
  filter(!is.na(mrk_increse_WASH_50)) %>%
  group_by(mrk_increse_WASH_50) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.WASH100 <- current.month.analysis %>%
  dplyr::select(mrk_increse_WASH_100) %>%
  filter(!is.na(mrk_increse_WASH_100)) %>%
  group_by(mrk_increse_WASH_100) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.wash <- bind_rows(supply.increase.WASH50, supply.increase.WASH100)

write.csv(supply.increase.wash, file = paste0("Outputs/supply_increase_WASH_",current_month,".csv"), row.names = FALSE)


## Supply increase: WATER ##
supply.increase.water50 <- current.month.analysis %>%
  dplyr::select(mrk_increse_water_50) %>%
  filter(!is.na(mrk_increse_water_50)) %>%
  group_by(mrk_increse_water_50) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.water100 <- current.month.analysis %>%
  dplyr::select(mrk_increse_water_100) %>%
  filter(!is.na(mrk_increse_water_100)) %>%
  group_by(mrk_increse_water_100) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.wash <- bind_rows(supply.increase.water50, supply.increase.water100)

write.csv(supply.increase.wash, file = paste0("Outputs/supply_increase_water_",current_month,".csv"), row.names = FALSE)





## Download REACH-specific packages


## Load libraries



## Load questionnaire



## 