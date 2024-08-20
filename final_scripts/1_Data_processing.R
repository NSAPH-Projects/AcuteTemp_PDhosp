
# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)
library(fst)
library(dplyr)

# Directory with PD hospitalization data
dir_pd <- "/n/dominici_nsaph_l3/Lab/projects/analytic/parkinsons_disease/"
list.files(dir_pd)
# [1] "parkinsons_2000.csv"     "parkinsons_2001.csv" ...

# Load each file and combine
pd_hosp <- rbindlist(lapply(2000:2016, function(y) {
  fread(paste0(dir_pd, "parkinsons_", y, ".csv"))
}))
# restrict to admission sources 1, 2, 7
pd_hosp <- pd_hosp[admission_source %in% c("1", "2", "7")]

# Column descriptions
head(pd_hosp)
# bene_id = beneficiary id
# admission_date = admission date

# Subset to first hospitalization
pd_first_hosp <-
  pd_hosp[order(bene_id, admission_date), lapply(.SD, first), by = bene_id]
pd_first_hosp[, .N]

#Subset first hosp to summer months in this case (May-Sept)
pd_first_hosp[, month := month(ymd(admission_date))]
pd_first_hosp_summer <- pd_first_hosp[month >= 5 & month <= 9, ]

#save file for later use and easy loading
write_fst(pd_first_hosp_summer, "data/pd_first_hosp_summer.fst")

##How to load an fst when needed later
pd_first_hosp_summer <- read_fst("data/pd_first_hosp_summer.fst", as.data.table = T)

##adjust date layout for summer data so it matches the denom data
pd_first_hosp_summer[, date := ymd(admission_date)]
head(pd_first_hosp_summer)
## add denom data to the pd_first_hosp_summer dataset 
##code from dmork code snippets on loading demonimator data (add pathway)
#load the denominator directory
dir_denom <- "/n/dominici_nsaph_l3/Lab/projects/analytic/denom_by_year/"
list.files(dir_denom)

#get info on columns
metadata_fst(paste0(dir_denom, list.files(dir_denom)[1]))

#read in the denom files and columns of interest
# Read in denominator files
years <- 2000:2016
vars <- c("qid", "hmo_mo", "dual", "dead", "fips", "statecode", "zcta", "poverty", 
          "popdensity", "medianhousevalue", "pct_owner_occ", "hispanic",
          "education", "smoke_rate", "mean_bmi") # add variables as needed

#bind everything to create denom data set, will require a large data allocation
denom <- rbindlist(lapply(years, function(y) {
  read_fst(paste0(dir_denom, "confounder_exposure_merged_nodups_health_",
                  y, ".fst"), columns = vars, as.data.table = TRUE)
}))

#rename the id column in the denom file so that it matches the pd hosp file
colnames(denom)[colnames(denom) == "qid"] <- "bene_id"

#merge the files based on bene_id
#remove duplicates in the denom data 
denom_xduplicates <- denom %>% distinct(bene_id, .keep_all = TRUE)
denom_pd_first_hosp_summer <- merge (pd_first_hosp_summer, denom_xduplicates, by = "bene_id")

#might want to save an fst just in case, not totally necessary though 
write_fst(denom_pd_first_hosp_summer, "data/denom_pd_first_hosp_summer.fst")

#we have already subset to the first hosp in summer so we can use the following 
#code to create the case and control days 


# Use Angela's code to do this:
# Use denom_pd_first_hosp_summer data to create case-crossover
# create a control record for each individual on same day of week
# for remaining weeks in each month (both earlier and later) 
# (eg hosp may 12; control may 5, 19, 26)
denom_pd_first_hosp_summer[, admission_date := ymd(date)]
denom_pd_first_hosp_summer[, month := month(ymd(admission_date))]

for (i in 1:6){
  tmp_back = copy(denom_pd_first_hosp_summer)
  tmp_forw = copy(denom_pd_first_hosp_summer)
  
  #tmp_back[, admission_date := admission_date - 7*i]
  tmp_back$admission_date = tmp_back$admission_date -7*i
  tmp_forw$admission_date = tmp_forw$admission_date + 7*i
  
  tmp_back$month_new = month(tmp_back$admission_date)
  tmp_back = tmp_back[tmp_back$month == tmp_back$month_new,]
  tmp_back$month_new = NULL
  
  tmp_forw$month_new = month(tmp_forw$admission_date)
  tmp_forw = tmp_forw[tmp_forw$month == tmp_forw$month_new, ]
  tmp_forw$month_new = NULL
  
  tmp = bind_rows(tmp_back, tmp_forw) 
  if (i==1){control = tmp}else{control = bind_rows(control,tmp)}
  rm(tmp_back);rm(tmp_forw);rm(tmp);gc()
  cat(i)
}

#create case and control variables 
control$case=0
denom_pd_first_hosp_summer$case=1

# Combine control/event dates and SAVE
PD_CCO <- rbind(denom_pd_first_hosp_summer, control)
write_fst(PD_CCO, "data/PD_CCO.fst")

#
setDT(PD_CCO)
PD_CCO[, date := admission_date]
setkey(PD_CCO, zip, date)
zips <- unique(denom_pd_first_hosp_summer$zip)

rm(control);gc()
rm(denom_pd_first_hosp_summer);gc()

#load exposure data
##the following code is from dmork code snippets, all credit to daniel mork 

# Required libraries
library(data.table)
library(lubridate)
library(weathermetrics)

# Exposures directory
dir_exposures <- "/n/dominici_lab_ro/lab/data/exposures/exposure/"
dir_temp <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/"

# Daily air pollution data, edit inputs to capture specific range of days
pm25 <- "pm25/PM25_v2" # pm25/PM25_v2, no2/NO2_v2, ozone/O3_v2
year_range <- 2000:2016
month_range <- 5:9 #formatC(1:5, width = 2, format = "d", flag = "0")

pm25 <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, pm25, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$year <- y
      dat$month <- m
      dat$day <- d
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))

pm25[, date := ymd(date)]
pm25$date <- as.character.Date(pm25$date)
PD_CCO[, date := ymd(date)]
PD_CCO$date <- as.character.Date(PD_CCO$date)

pm25 <- pm25[year(date) %in% years & ZIP %in% zips]
setnames(pm25, c("zip", "pm25", "STATE", "year", "month", "day", "date"))
PD_CCO$zip <- as.character(PD_CCO$zip)
pm25$zip <- as.character(pm25$zip)
colnames(PD_CCO)
colnames(pm25)
PD_CCO <- merge(PD_CCO, pm25[, c("zip", "date", "STATE", "day", "pm25")], by = c("zip", "date"), all.x = TRUE)
write_fst(PD_CCO, "data/PD_CCO_PM25.fst")



no2 <- "no2/NO2_v2" # pm25/PM25_v2, no2/NO2_v2, ozone/O3_v2
year_range <- 2000:2016
month_range <- 5:9 #formatC(1:5, width = 2, format = "d", flag = "0")

no2 <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, no2, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$year <- y
      dat$month <- m
      dat$day <- d
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))


no2[, date := ymd(date)]
no2 <- no2[year(date) %in% years & ZIP %in% zips]
setnames(no2, c("zip", "no2", "STATE", "year", "month", "day", "date"))
no2$date <- as.character.Date(no2$date)
PD_CCO$zip <- as.character(PD_CCO$zip)
no2$zip <- as.character(no2$zip)
no2$date <- as.character.Date(no2$date)

PD_CCO <- merge(PD_CCO, no2[, c("zip", "date", "no2")], by = c("zip", "date"), all.x = TRUE)

ozone <- "ozone/O3_v2" # pm25/PM25_v2, no2/NO2_v2, ozone/O3_v2
year_range <- 2000:2016
month_range <- 5:9 #formatC(1:5, width = 2, format = "d", flag = "0")

ozone <- rbindlist(lapply(year_range, function(y) {
  rbindlist(lapply(month_range, function(m) {
    rbindlist(lapply(1:days_in_month(ymd(paste0(y, "/", m, "/1"))), function(d) {
      dat <- readRDS(paste0(dir_exposures, ozone, "/daily/",
                            y, sprintf("%02d", m), sprintf("%02d", d), ".rds"))
      dat$year <- y
      dat$month <- m
      dat$day <- d
      dat$date <- ymd(paste0(y, sprintf("%02d", m), sprintf("%02d", d)))
      dat
    }))
  }))
}))


ozone[, date := ymd(date)]
ozone <- ozone[year(date) %in% years & ZIP %in% zips]
setnames(ozone, c("zip", "ozone", "STATE", "year", "month", "day", "date"))
ozone$date <- as.character.Date(ozone$date)
PD_CCO$zip <- as.character(PD_CCO$zip)
ozone$zip <- as.character(ozone$zip)
ozone$date <- as.character.Date(ozone$date)
PD_CCO_exp <- merge(PD_CCO, ozone[, c("zip", "date", "ozone")], by = c("zip", "date"), all.x = TRUE)

write_fst(PD_CCO_exp, "data/PD_CCO_exp.fst")

# create heat exposures and lagged heat exposure


# heat data
# Daily max temperature, min humidity, heat index data
dir_temp <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/"
year_range <- 2000:2016
month_range <- 4:9
# load gridmet max temp
max_temp <- rbindlist(lapply(year_range, function(y) {
  load(paste0(dir_temp, 
              "maximum_air_temperature/", y,
              "_maximum_air_temperature_by_zip.RData"))
  df$zip <- rownames(df)
  setDT(df)
  max_temp <- melt(df, id.vars = "zip", variable.name = "date", value.name = "max_temp")
  max_temp[, date := ymd(date)]
  max_temp[, zip := sprintf("%05d", as.integer(zip))]
}))

# load gridmet min humidity
min_humid <- rbindlist(lapply(year_range, function(y) {
  load(paste0(dir_temp, 
              "minimum_relative_humidity/", y,
              "_minimum_relative_humidity_by_zip.RData"))
  df$zip <- rownames(df)
  setDT(df)
  min_humid <- melt(df, id.vars = "zip", variable.name = "date", value.name = "min_humid")
  min_humid[, date := ymd(date)]
  min_humid[, zip := sprintf("%05d", as.integer(zip))]
}))

# calculate heat index
heat_index <- merge(max_temp, min_humid, by = c("zip", "date"))
heat_index[, heat_index := 
             heat.index(max_temp - 273.15, 
                        rh = min_humid,
                        temperature.metric = "celsius", 
                        output.metric = "celsius", round = 3)]
write_fst(heat_index, "data/heat_index.fst")


# create lagged exposure
setkey(heat_index, zip, date)
heat_index[, paste0("HI_lag_", 0:14) := shift(heat_index, 0:14, type = "lag"), by = zip]


# merge with hosp data
PD_CCO_exp[, date := ymd(admission_date)]
PD_CCO_heat_merged_data <- merge(PD_CCO_exp, heat_index, 
                                 by.x = c("zip", "date"),
                                 by.y = c("zip", "date"))

data_complete <- PD_CCO_heat_merged_data[complete.cases(PD_CCO_heat_merged_data
                                      [, .(HI_lag_0, HI_lag_1, HI_lag_2, HI_lag_3,
                                      HI_lag_4, HI_lag_5, HI_lag_6, HI_lag_7,
                                      HI_lag_8, HI_lag_9, HI_lag_10, HI_lag_11, 
                                      HI_lag_12, HI_lag_13, HI_lag_14, pm25, no2, ozone)])]

# save merged dataset!
write_fst(PD_CCO_heat_merged_data, "data/PD_CCO_heat_merged_data.fst")

#Merge in climate type data

##Add in the temperature zones to the data 
library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)

##read in climate data (from Angela Stegmueller's code)

climate <- fread("/n/dominici_nsaph_l3/Lab/projects/temperature-adrd-casecrossover/data/Koppen_counties.csv")
setnames(climate, c("fips", "Koppen"))
climate[, Koppen_s := substr(Koppen, 1, 1)]
data_complete_climate <- merge(data_complete, climate, by = "fips")
data_complete_climate$Koppen_s <- factor(data_complete_climate$Koppen_s, c("C", "D", "B", "A"),
                                     c("Temperate", "Continental", "Arid", "Tropical"))


#Save that file!!!
write_fst(data_complete_climate, "data/data_complete_climate.fst")
