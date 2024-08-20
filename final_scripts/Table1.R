# Install and load necessary packages
#install.packages("tableone")
library(tableone)
                               
                                 
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)
                                 
#Edit the dataset to remove rows with HI values below 0 
data <- data[data$HI_lag_0 >= 0, ]

# Define the variables to include in the table
 vars_to_include <- c("age", "sex", "race", "dual",
          "poverty", "popdensity", "medianhousevalue", "pct_owner_occ", 
          "hispanic", "education", "smoke_rate", "mean_bmi", "pm25",
          "no2", "ozone", "HI_lag_0", "Koppen_s")
                              
 # Define labels for the variables
    labels <- c(
        age = "Age (years)",
        sex = "Sex",
        race = "Race",
        hispanic = "% Hispanic",
        mean_bmi = "Body Mass Index (BMI)",
        smoke_rate = "Smoking Rate",
        pct_owner_occ = "% Owner Occupied")
                               
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
                               
                               
                                 
                                                                  