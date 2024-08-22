
# Install and load necessary packages
library(tableone)
library(data.table)
library(fst)
library(tidyverse)

# Load data
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)
                                 
# Edit the dataset to remove rows with HI values below 0 
data <- data[data$HI_lag_0 >= 0, ]

# Define age categories
data[, age_cat := cut(age, 
                   breaks = c(65, 74, 84, Inf), 
                   labels = c("65-74", "75-84", "85+"), 
                   right = FALSE)]

# Remove 5 people with undefined age
data <- data[!is.na(age_cat)]

# Make race, sex, dual factors
data[, race := as.factor(race)]
data[, sex := as.factor(sex)]
data[, dual := as.factor(dual)]

# make race values 0 and 6 --> 3 (all in "other" category)
data[race %in% c(0, 6), race := 3]

# Define the variables to include in the table
# vars_to_include <- c("age", "sex", "race", "dual",
#           "poverty", "popdensity", "medianhousevalue", "pct_owner_occ", 
#           "hispanic", "education", "smoke_rate", "mean_bmi", "pm25",
#           "no2", "ozone", "HI_lag_0", "Koppen_s")
vars_to_include <- c("age_cat", "sex", "race", "dual", "HI_lag_0")
                              
# Define labels for the variables
# labels <- c(
#     age = "Age (years)",
#     sex = "Sex",
#     race = "Race",
#     hispanic = "% Hispanic",
#     mean_bmi = "Body Mass Index (BMI)",
#     smoke_rate = "Smoking Rate",
#     pct_owner_occ = "% Owner Occupied")
labels <- c(
  age = "Age (years)",
  sex = "Sex",
  race = "Race")
                               
# Create Table 1 for cases in different regions with specified labels
tab1_regions <- CreateTableOne(vars = vars_to_include, 
     data = data[data$case == 1, ], 
     strata = "Koppen_s", 
     test = FALSE)
                               
# Print Table 1 for cases in different regions with specified labels
print(tab1_regions)

# Create Table 1 for cases in all regions with specified labels
tab1_overall <- CreateTableOne(vars = vars_to_include, 
      data = data[data$case == 1, ], 
      test = FALSE)
                               
# Print Table 1 for cases in all regions with specified labels
print(tab1_overall)



################################################

# get heat index values for each stratum

#---------- age

age_koppen <- data %>%
  filter(case == 1) %>%
  group_by(age_cat) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")")) %>%
  mutate(Koppen_s = "Nationwide") %>%
  select(c(Koppen_s, age_cat, HI))
age_all <- data %>%
  filter(case == 1) %>%
  group_by(Koppen_s, age_cat) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")"))
rbind(age_koppen, age_all)


#---------- sex

sex_koppen <- data %>%
  filter(case == 1) %>%
  group_by(sex) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")")) %>%
  mutate(Koppen_s = "Nationwide") %>%
  select(c(Koppen_s, sex, HI))
sex_all <- data %>%
  filter(case == 1) %>%
  group_by(Koppen_s, sex) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")"))
rbind(sex_koppen, sex_all)


#---------- race

race_koppen <- data %>%
  filter(case == 1) %>%
  group_by(race) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")")) %>%
  mutate(Koppen_s = "Nationwide") %>%
  select(c(Koppen_s, race, HI))
race_all <- data %>%
  filter(case == 1) %>%
  group_by(Koppen_s, race) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")"))
print(rbind(race_koppen, race_all), n = 50)


#---------- Medicaid

dual_koppen <- data %>%
  filter(case == 1) %>%
  group_by(dual) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")")) %>%
  mutate(Koppen_s = "Nationwide") %>%
  select(c(Koppen_s, dual, HI))
dual_all <- data %>%
  filter(case == 1) %>%
  group_by(Koppen_s, dual) %>%
  summarise(HI = paste0(round(mean(HI_lag_0), 1), " (", 
                        round(sd(HI_lag_0), 1), ")"))
rbind(dual_koppen, dual_all)
